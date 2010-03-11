;;; yaham-hugs.el --- Hugs specific things for Yaham

;; Copyright (C) 2007 Ian Zimmerman <itz@madbat.mine.nu>

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the conditions spelled out in
;; the file LICENSE are met.

;; $Id: yaham-hugs.el 90 2007-09-03 05:26:42Z itz $

(require 'yaham-proc)
(require 'yaham)
(require 'easymenu)

(defgroup yaham-hugs nil
  "Hugs specific things for Yaham."
  :group 'yaham
  :prefix "yaham-hugs-")

(defcustom yaham-hugs-mode-hook nil
  "Normal hook which runs after a buffer enters Yaham Hugs mode."
  :group 'yaham-hugs
  :type 'hook)

(defcustom yaham-hugs-program "hugs"
  "*The name of the Hugs interpreter executable."
  :group 'yaham-hugs
  :type 'string)

(defcustom yaham-hugs-program-options '("-98" "+t")
  "*Extra options to pass to the Hugs interpreter executable."
  :group 'yaham-hugs
  :type '(repeat string))


(defconst yaham-hugs-error-format
  '(("^ERROR[ \t]+\"\\([^\"]*\\)\":\\([0-9]+\\)[ \t]+-" 1 2))
  "Format of a Hugs error message line.")

(defconst yaham-hugs-font-lock-keywords
  `(("\\<forall\\>" . font-lock-keyword-face)
    ("\\<mdo\\>" . font-lock-builtin-face)
    ,@yaham-font-lock-keywords))

(defconst yaham-hugs-prompt "^yaham:[^ >]+> "
  "Regular expression to expect as prompt in Hugs interpreter buffer.")

(defconst yaham-hugs-show-if-regexp "^\\(ERROR\\|Unknown\\)\\>"
  "Show the Hugs process buffer if this pattern occurs in the output.")

(defconst yaham-hugs-mode-map
  (let ((kmap (copy-keymap yaham-mode-map)))
    (define-key kmap "\C-c\C-c" 'yaham-hugs-cd)
    (define-key kmap "\C-c\C-d" 'yaham-hugs-describe-symbol)
    (define-key kmap "\C-c\C-e" 'yaham-hugs-eval-expr)
    (define-key kmap "\C-c\C-f" 'yaham-hugs-add-file)
    (define-key kmap "\C-c\C-m" 'yaham-hugs-load-module)
    (define-key kmap "\C-c\C-s" 'yaham-hugs-set-module)
    (define-key kmap "\C-c\C-p" 'yaham-hugs-add-to-path)
    (define-key kmap "\M-\C-i" 'yaham-proc-complete-tag-or-symbol)
    (define-key kmap [?\C-c C-mouse-2] 'yaham-hugs-describe-symbol-at-mouse)
    kmap))

(easy-menu-define yaham-hugs-mode-menu yaham-hugs-mode-map "Hugs submenu to use in Yaham buffers."
  '("Hugs"
    ["Start Hugs" yaham-hugs-run :help "Start the Hugs interpreter if not already started"] 
    ["Change Directory" yaham-hugs-cd :help "Change the current directory of the Hugs interpreter"]
    ["Add Directory to Path" yaham-hugs-add-to-path :help "Add a directory to Hugs module search path"]
    ["Add File" yaham-hugs-add-file :help "Add a file into Hugs working set"]
    ["Load Module" yaham-hugs-load-module :help "Load a module into Hugs working set, discarding others"]
    ["Describe Symbol" yaham-hugs-describe-symbol :help "Ask Hugs to describe a symbol"]
    ["Evaluate Expression" yaham-hugs-eval-expr :help "Ask Hugs to evaluate the marked expression"]
    ["Set Module" yaham-hugs-set-module :help "Ask Hugs to change its current module"]))

(defsubst yaham-hugs-guess-libdir ()
  (catch 'found
    (let ((fullhugs (shell-command-to-string (concat "which " yaham-hugs-program))))
      (string-match "\\`[ \t\n]*\\(.*\\)[ \t\n]*\\'" fullhugs)
      (setq fullhugs (match-string-no-properties 1 fullhugs))
      (if (string-match "\\`\\(.*\\)/bin/[^/]+\\'" fullhugs)
          (let ((dir (concat (match-string-no-properties 1 fullhugs) "/lib/hugs")))
            (if (and (file-directory-p dir)
                     (file-directory-p (concat dir "/packages")))
                (throw 'found dir)))))
    "/usr/local/lib/hugs"))

(defvar yaham-hugs-library-directory (yaham-hugs-guess-libdir)
  "Directory where to look for modules packaged with Hugs.
It can usually be autodetected but this is not completely foolproof,
therefore this is a `defvar' and not a `defconst'.")


(defun yaham-hugs-run (&optional noshow)
  "Start the Hugs interpreter if not already started.
From a program, if the interpreter is already running, just show its
window, unless NOSHOW is non-nil."
  (interactive)
  (yaham-proc-run
   "Hugs"
   yaham-hugs-program
   (cons "+pyaham:%s> " yaham-hugs-program-options)
   yaham-hugs-error-format
   yaham-hugs-prompt
   noshow))

(defsubst yaham-hugs-wait-for-prompt ()
  (yaham-proc-wait-for-prompt "Hugs" yaham-hugs-prompt))

(defsubst yaham-hugs-send-string (fmt &rest args)
  (apply 'yaham-proc-send-string "Hugs" fmt args))

(defsubst yaham-hugs-get-output (snarf-fun fmt &rest args)
  (apply 'yaham-proc-get-output "Hugs" yaham-hugs-prompt snarf-fun fmt args))

(defsubst yaham-hugs-maybe-show ()
  (yaham-proc-maybe-show "Hugs" yaham-hugs-prompt yaham-hugs-show-if-regexp))

(defun yaham-hugs-expand-paths (paths)
  "Given a list of Hugs lookup paths, return corresponding list of directories.
A Hugs lookup path is either a plain directory file name, possibly with
leading metavariables {Home} or {Hugs}, or else has the form DIR/* which
stands for all the immediate subdirectories of DIR.  This function
substitutes the subdirectories."
  (apply 'append
         (mapcar (lambda (p)
                   (cond
                    ((string-match "\\`{Home}/" p)
                     (setq p (concat "~/" (substring p 7))))
                    ((string-match "\\`{Hugs}/" p)
                     (setq p (expand-file-name (substring p 7) yaham-hugs-library-directory)))
                    (t nil))
                   (if (string-match "/\\*\\'" p)
                       (yaham-proc-directory-subdirs
                        (substring p 0 -2))
                     (list p)))
                 paths)))

(defsubst yaham-hugs-snarf-search-path ()
  "Ask Hugs what its current module search path is."
  (yaham-hugs-run t)
  (yaham-hugs-get-output
   (lambda ()
     (re-search-forward "^Search path *: *-P\\(.*\\)$")
     (match-string-no-properties 1)) ":set\n"))

(defsubst yaham-hugs-search-paths ()
  (let* ((pp (yaham-hugs-snarf-search-path))
         (psplit (split-string pp ":")))
    (yaham-hugs-expand-paths psplit)))    

(defun yaham-hugs-snarf-files ()
  "Ask Hugs what files it currently has loaded."
  (yaham-hugs-run t)
  (yaham-hugs-get-output
   (lambda ()
     (let ((files nil))
       (re-search-forward "^Hugs session for:\n")
       (while (not (eobp))
         (let ((p (point)))
           (end-of-line)
           (push (buffer-substring-no-properties p (point)) files)
           (forward-line 1)))
       files)) ":info\n"))

(defun yaham-hugs-loaded-modules ()
  (mapcar 'yaham-proc-file->module (yaham-hugs-snarf-files)))

(defsubst yaham-hugs-check-sources (&rest newnames)
  "For each FN known to Hugs, and NEWNAMES, check if FN is in a modified buffer."
  (mapc 'yaham-proc-check-source (append newnames (yaham-hugs-snarf-files))))

;; defsubst for speed
(defsubst yaham-hugs-munge-sym (start)
  (let ((end (point)))
    (cond
     ((char-equal (char-before) ?\) )
      (skip-chars-backward "^(")
      (buffer-substring-no-properties (point) (1- end)))
     (t
      (skip-chars-backward "^." start)
      (buffer-substring-no-properties (point) end)))))

(defun yaham-hugs-snarf-symbols ()
  "Ask Hugs for the names of all symbols it knows about."
  (yaham-hugs-run t)
  (yaham-hugs-get-output
   (lambda ()
     (let ((symbols nil))
       (while (not (eobp))
         ;; this needs to be fast, so no regexp match
         (let ((p (point)))
           (skip-chars-forward "^ ")
           (push (yaham-hugs-munge-sym p) symbols)
           (forward-line 1)))
       symbols)) ":browse all\n"))


(defun yaham-hugs-add-file (filename)
  "Add a file into Hugs working set.
Normally, add the file corresponding to the current buffer,
however, with a non-nil prefix argument ANY, prompts for the file to add."
  (interactive (list (yaham-read-file "Add file: ")))
  (yaham-hugs-run t)
  (yaham-hugs-check-sources filename)
  (yaham-hugs-send-string ":also \"%s\"\n" filename)
  (yaham-hugs-maybe-show))

(defun yaham-hugs-load-module (modname)
  "Load a module into Hugs working set.
This means all other modules are discarded except the Hugs core modules.
Normally, load the module named at point, or the one defined in current buffer,
however, with a non-nil prefix argument ANY, prompts for the module to load."
  (interactive (list (yaham-proc-read-module "Load module: ")))
  (yaham-hugs-run t)
  (yaham-hugs-check-sources)
  (yaham-hugs-send-string ":load %s\n" modname)
  (yaham-hugs-maybe-show))

(defun yaham-hugs-eval-any-expr (expr)
  "Evaluate the expression EXPR \(interactively, prompt for one\)."
  (interactive (list (read-from-minibuffer
                      "Eval: " nil nil nil
                      'read-expression-history)))
  ;; must replace newlines in expr for Hugs
  (let* ((i 0) (l (length expr)))
    (while (< i l)
      (if (= (aref expr i) ?\n  ) (aset expr i ?/  ))
      (setq i (1+ i))))
  (yaham-hugs-run)
  (yaham-hugs-send-string "%s\n" expr)
  (yaham-hugs-maybe-show))

(defun yaham-hugs-eval-region (start end)
  "Evaluate the buffer substring between START and END \(interactively, the region\)."
  (interactive "r")
  (yaham-hugs-eval-any-expr (buffer-substring-no-properties start end)))

(defun yaham-hugs-eval-expr (&optional any)
  "Evaluate an expression.
If the region is visible and ANY is nil, evaluate the region.
Otherwise, prompt for an expression to evaluate."
  (interactive "P")
  (if (and (yaham-region-visible-p) (not any))
      (call-interactively 'yaham-hugs-eval-region)
    (call-interactively 'yaham-hugs-eval-any-expr)))

(defun yaham-hugs-describe-any-symbol (sym)
  (interactive (list (yaham-proc-read-symbol "Describe symbol: ")))
  (yaham-hugs-run)
  (yaham-hugs-send-string ":info %s\n" sym)
  (yaham-hugs-maybe-show))
  
(defun yaham-hugs-describe-symbol (p &optional any)
  "Ask Hugs to describe a symbol.
Normally, the symbol is the one at point, however,
with a non-nil prefix argument ANY, prompt for the symbol to ask about."
  (interactive "d\nP")
  (if any (call-interactively 'yaham-hugs-describe-any-symbol)
    (save-excursion
      (goto-char p)
      (yaham-hugs-describe-any-symbol (yaham-symbol-at-point)))))

(defun yaham-hugs-describe-symbol-at-mouse (e)
  "Describe the symbol on which mouse was clicked."
  (interactive "@e")
  (mouse-set-point e)
  (call-interactively 'yaham-hugs-describe-symbol))

(defun yaham-hugs-cd (d)
  "Change the current directory of the Hugs interpreter."
  (interactive "DDirectory: ")
  (yaham-hugs-run t)
  (let* ((dexp (expand-file-name d))
         (obuf (yaham-hugs-send-string ":cd \"%s\"\n" dexp)))
    (with-current-buffer obuf (cd dexp))))

(defun yaham-hugs-add-to-path (d)
  "Add directory D to the Hugs module search path."
  (interactive "DDirectory: ")
  (yaham-hugs-run t)
  (if (and (< 0 (length d))
           (string-match "/\\'" d))
      (setq d (substring d 0 -1)))
  (let* ((oldpath (yaham-hugs-snarf-search-path))
         (newpath (concat oldpath ":" (expand-file-name d))))
    (yaham-hugs-send-string ":set +P%s\n" newpath)))

(defun yaham-hugs-set-module (mod)
  "Ask Hugs to change its current module."
  (interactive (list (yaham-proc-read-module "Set current module: ")))
  (yaham-hugs-run t)
  (yaham-hugs-send-string ":module %s\n" mod)
  (yaham-hugs-maybe-show))

(defun yaham-hugs-reload ()
  "Ask Hugs to reload all modules."
  (interactive)
  (yaham-hugs-run t)
  (yaham-hugs-send-string ":reload\n"))

;;;###autoload
(defun yaham-hugs-mode ()
  "A specialization of `yaham-mode' for Hugs."
  (interactive)
  (delay-mode-hooks
    (yaham-mode))
  (setq major-mode 'yaham-hugs-mode)
  (setq mode-name "Yaham-Hugs")
  (use-local-map yaham-hugs-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(yaham-hugs-font-lock-keywords
          nil nil ((?_ . "w") (?' . "w"))
          beginning-of-defun
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-keywords
           . yaham-font-lock-syntactic-keywords)))
  (setq yaham-ffap-path-function 'yaham-hugs-search-paths)
  (setq yaham-proc-list-modules-function 'yaham-hugs-loaded-modules)
  (setq yaham-proc-list-symbols-function 'yaham-hugs-snarf-symbols)
  (imenu-add-menubar-index)
  (run-mode-hooks 'yaham-hugs-mode-hook))

(provide 'yaham-hugs)


;; Local Variables:
;; generated-autoload-file: "/home/itz/src/yaham/yaham-autoloads.el"
;; End:

;;; yaham-hugs.el ends here
