;;; yaham-proc.el --- common interpreter subprocess things for Yaham

;; Copyright (C) 2007 Ian Zimmerman <itz@madbat.mine.nu>

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the conditions spelled out in
;; the file LICENSE are met.

;; $Id: yaham-proc.el 88 2007-09-02 15:10:05Z itz $

(require 'compile)
(require 'comint)
(require 'yaham)

(defgroup yaham-proc nil
  "Handling a Haskell interpreter in a buffer."
  :group 'yaham
  :prefix "yaham-proc-")

(defcustom yaham-proc-display-buffer-function 'display-buffer
  "*Function of one argument used by `yaham-proc-run' to display the interpreter buffer."
  :group 'yaham-proc
  :type '(radio
          (function-item :doc "Show in some window" display-buffer)
          (function-item :doc "Show in this window" switch-to-buffer)
          (function-item :doc "Show in another window and select" switch-to-buffer-other-window)
          (function-item :doc "Show in some window and select" pop-to-buffer)
          (function :tag "Your own function")))

(defvar yaham-proc-symbol-history nil
  "History of inputs for `yaham-proc-read-symbol'.")

(defvar yaham-proc-list-symbols-function nil
  "A function of no arguments returning the list of currently defined symbols.")
(make-variable-buffer-local 'yaham-proc-list-symbols-function)

(defun yaham-proc-read-symbol (prompt)
  (let* ((symbols (funcall yaham-proc-list-symbols-function))
         (sym-at-pt (yaham-symbol-at-point))
         (default (if (member sym-at-pt symbols) sym-at-pt))
         (sym-alist (mapcar 'list symbols))
         (result (completing-read prompt sym-alist nil t nil
                                  'yaham-proc-symbol-history default)))
    (if (string= result "") default result)))

(defvar yaham-proc-module-history nil
  "A history list for `yaham-hugs-read-module'.")

(defvar yaham-proc-list-modules-function nil
  "A function of no arguments returning the list of currently loaded modules.")
(make-variable-buffer-local 'yaham-proc-list-modules-function)

(defconst yaham-proc-module-hash
  (make-hash-table :test 'equal)
  "Hash table to cache the mapping from file names to modules names.
Since the Hugs interpreter provides no documented shortcut for this,
we must open the file and find the module declaration, which is
relatively expensive.")

;; Hugs really should have a way of answering this
(defun yaham-proc-file->module (filename)
  "Guess what module is defined by FILENAME."
  (let* ((cached (gethash filename yaham-proc-module-hash))
         (stamp (cdr cached))
         (mtimelist (nth 5 (file-attributes filename)))
         (stale
          (or (not stamp)
              (< (nth 0 stamp) (nth 0 mtimelist))
              (and (= (nth 0 stamp) (nth 0 mtimelist))
                   (< (nth 1 stamp) (nth 1 mtimelist))))))
    (if (not stale) (car cached)
      (let* ((bf (generate-new-buffer filename))
             (found-mod 
              (with-current-buffer bf
                (insert-file-contents filename)
                (yaham-mode)
                (or (yaham-this-module) ""))))
        (kill-buffer bf)
        (puthash filename (cons found-mod mtimelist) yaham-proc-module-hash)
        found-mod))))      

(defsubst yaham-proc-default-modname ()
  (or (yaham-modpath-at-point) (yaham-this-module)))

(defun yaham-proc-read-module (prompt)
  "Read a name of a module currently loaded in Hugs from the minibuffer.
The usual conventions apply to PROMPT, similar to `read-from-minibuffer'.
If the file in the current buffer defines a loaded module, it is the default."
  (let* ((modules (funcall yaham-proc-list-modules-function))
         (this-module (yaham-proc-default-modname))
         (default (if (member this-module modules) this-module)))
    (if (and default (not current-prefix-arg)) default
      (let* ((module-alist (mapcar (lambda (m) (list m)) modules))
             (result (completing-read prompt module-alist nil t nil
                                      'yaham-proc-module-history default)))
        (if (string= result "") default result)))))

(defun yaham-proc-directory-subdirs (d)
  "Return a list of all immediate subdirectories of D, with full names.
That includes symlinks in D which point to directories."
  (let ((fs
         (condition-case nil
             (directory-files d t)
           (error nil)))
        (subdirs nil))
    (while fs
      (let ((f (pop fs)))
        (unless (or (string-match "/.\\'" f) (string-match "/..\\'" f))
          (let ((sd d) (kind (nth 0 (file-attributes f))))
            (while (stringp kind)
              (let ((realname (expand-file-name kind sd)))
                (setq kind (nth 0 (file-attributes realname)))
                (setq sd (file-name-directory realname))))
            (if kind (push f subdirs))))))
    subdirs))         

;; like comint-check-source, but fixed.
;; sigh.
(defun yaham-proc-check-source (fname)
  (let ((buff (find-buffer-visiting fname)))
    (if (and buff
	     (buffer-modified-p buff)
	     (y-or-n-p (format "Save buffer %s first? " (buffer-name buff))))
	;; save BUFF.
        (with-current-buffer buff (save-buffer)))))

(defun yaham-proc-wait-for-prompt (name prompt)
  "Don't return until process NAME prompts with regexp PROMPT."
  (let* ((b (get-buffer (format "*%s*" name)))
         (p (get-buffer-process b)))
    (with-current-buffer b
      (save-excursion
        (goto-char (point-max))
        (while (not (progn
                      (forward-line 0)
                      (looking-at (concat prompt " *$"))))
          (accept-process-output p)
          (goto-char (point-max)))))))

(defun yaham-proc-make-font-lock-keywords ()
  (mapcar
   (lambda (pat)
     (append
      (list
       (nth 0 pat)
       (list (nth 1 pat) 'font-lock-warning-face t)
       (list (nth 2 pat) 'font-lock-variable-name-face))
      (if (nth 3 pat) (list (list (nth 3 pat) 'font-lock-type-face)))))
   compilation-error-regexp-alist))

(defun yaham-proc-run (name prog options errpats initial-prompt &optional noshow)
  "Run the Haskell interpreter PROG with OPTIONS as process NAME.
If the interpreter is already running, just show its window, unless
NOSHOW is non-nil.  ERRPATS the local value of
`compilation-error-regexp-alist'.  Wait for INITIAL-PROMPT to appear,
then show the buffer using `yaham-proc-display-buffer-function'."
  (let* ((bname (format "*%s*" name))
         (existing (get-buffer bname))
         (proc (when existing (get-buffer-process existing))))
    (cond
     ((or (not existing) (not proc))
      (let ((interp (apply 'make-comint name prog nil options)))
        (with-current-buffer interp
          (compilation-shell-minor-mode 1)
          (setq comint-scroll-to-bottom-on-output t)
          (setq comint-scroll-show-maximum-output t)
          (set (make-local-variable 'compilation-error-regexp-alist) errpats)
          (set (make-local-variable 'font-lock-defaults) '((yaham-proc-make-font-lock-keywords)))
          (let ((proc (get-buffer-process (current-buffer))))
            ;; wait for the prompt to appear
            (yaham-proc-wait-for-prompt name initial-prompt)
            ;; maintain parsed errors marker
            (set-process-filter
             proc (lambda (p s)
                    (condition-case nil
                        (with-current-buffer (process-buffer p)
                          (let ((cpe (marker-position compilation-parsing-end)))
                            (funcall 'comint-output-filter p s)
                            (set-marker compilation-parsing-end cpe)))
                      (error nil))))
            (set-process-query-on-exit-flag proc nil))))
      (funcall yaham-proc-display-buffer-function bname))
     ((not noshow)
      (funcall yaham-proc-display-buffer-function bname))
     (t nil))))

(defun yaham-proc-send-string (name fmt &rest args)
  "Send a command string to the interpreter NAME.  Return its buffer."
  (let ((b (get-buffer (format "*%s*" name)))
        (cmd (apply 'format fmt args)))
    (with-current-buffer b
      (save-excursion
        (goto-char (process-mark (get-buffer-process b)))
        (insert-before-markers cmd)))
    (comint-send-string b cmd)
    b))

(defun yaham-proc-get-output (name prompt snarf-fun fmt &rest args)
  "Send a command string built with FMT and ARGS to the interpreter NAME.
Then narrow the process buffer to just the output from the command,
call SNARF-FUN with no arguments and return its result.
PROMPT is the interpreter prompt regexp which serves to delimit the output.
As a side effect also delete the output and the preceding prompt from
the process buffer to reduce clutter.
This slightly odd interface avoids wasting space to build up the result
in a string if the output is large."
  (let* ((b (get-buffer (format "*%s*" name)))
         (window-start-alist nil)
         (old-end (with-current-buffer b (point-max))))
    (walk-windows
     (function
      (lambda (w)
        (if (eq (window-buffer w) b)
            (setq window-start-alist
                  (cons (cons w (window-start w)) window-start-alist))))))
    (apply 'yaham-proc-send-string name fmt args)
    (with-current-buffer b
      (unwind-protect
          (progn
            (yaham-proc-wait-for-prompt name prompt)
            (save-excursion
              (goto-char (point-max))
              (forward-line 0)
              (let ((end (point)))
                (goto-char old-end)
                (forward-line 1)
                (save-restriction
                  (narrow-to-region (point) end)
                  (funcall snarf-fun)))))
        (delete-region old-end (point-max))
        (mapc
         (lambda (as)
           (let ((w (car as))
                 (ws (cdr as)))
             (set-window-start w ws)))
         window-start-alist)))))

(defun yaham-proc-maybe-show (name prompt regexp)
  "Conditionally display the process buffer of process NAME.
Wait for its prompt PROMPT, then look for REGEXP in the most
recent chunk of output.  If found, display the buffer using
`yaham-proc-display-buffer-function'."
  (yaham-proc-wait-for-prompt name prompt)
  (let* ((b (get-buffer (format "*%s*" name)))
         (show
          (with-current-buffer b
            (save-excursion
              (goto-char (point-max))
              (forward-line 0)
              (let ((end (point)))
                (re-search-backward prompt)
                (forward-line 1)
                (re-search-forward regexp end t))))))
    (if show (funcall yaham-proc-display-buffer-function b))))

;; Yet another autotype feature, but the existing ones don't really fit
;; Modelled after complete-tag
(defun yaham-proc-complete-symbol ()
  "Perform completion on the Haskell symbol at or before point.
Completes to the set of names returned by `yaham-proc-list-symbols-function',
which usually interacts with an interactive Haskell environment in some way."
  (interactive)
  (let* ((pattern
          (or (yaham-symbol-at-point) ""))
         (symbols (sort (funcall yaham-proc-list-symbols-function) 'string<))
         (pairs (mapcar 'list symbols))
         (completion (try-completion pattern pairs nil)))
    (cond
     ((eq completion t))
     ((null completion)
      (message "Can't find completion for \"%s\"" pattern)
      (ding))
     ((not (string= pattern completion))
      (delete-region (- (point) (length pattern)) (point))
      (insert completion))
     (t
      (message "Making completion list...")
      (with-output-to-temp-buffer "*Completions*"
        (display-completion-list
         (all-completions pattern pairs nil)))
      (message "Making completion list...done")))))

;;  bind this one to M-TAB
(defun yaham-proc-complete-tag-or-symbol (arg)
  "Complete at point using `complete-tag' or other mechanism.
The action depends on ARG \(interactively, the prefix argument\),
and on the value of `last-command'.  With a non-nil ARG, run
`yaham-proc-complete-symbol'.  Without ARG, run `complete-tag'
unless `last-command' is `yaham-proc-complete-symbol', in that
case repeat it.
This is similar to \\[complete-symbol], but it replaces the binding
of `info-complete-symbol' which is unlikely to be useful in a buffer
with Haskell code."
  (interactive "P")
  (cond
   ((or arg (eq last-command 'yaham-proc-complete-symbol))
    (setq this-command 'yaham-proc-complete-symbol)
    (yaham-proc-complete-symbol))
   (t
    (complete-tag))))

(provide 'yaham-proc)


;; Local Variables:
;; generated-autoload-file: "/home/itz/src/yaham/yaham-autoloads.el"
;; End:

;;; yaham-proc.el ends here
