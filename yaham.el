;;; yaham.el -- simple haskell mode specialized for ghc

;; Copyright (C) 2007 Ian Zimmerman <itz@madbat.mine.nu>

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the conditions spelled out in
;; the file LICENSE are met.

;; $Id: yaham.el 90 2007-09-03 05:26:42Z itz $

(eval-when-compile
  (require 'newcomment)
  (require 'regexp-opt))

(require 'imenu)
(require 'easymenu)
(require 'thingatpt)

(defgroup yaham nil
  "Editing Haskell code while staying sane."
  :group 'languages
  :prefix "yaham-")

(defcustom yaham-mode-hook nil
  "Normal hook which runs after a buffer enters Yaham mode."
  :group 'yaham
  :type 'hook)

(defcustom yaham-ffap-suffix ".hs"
  "*The preferred suffix for the file name inferred by \\[yaham-ffap]."
  :group 'yaham
  :type 'string)

(defcustom yaham-ffap-path nil
  "*A list of directories where to look for the files inferred by \\[yaham-ffap].
The current directory is always searched independently of this variable."
  :group 'yaham
  :type '(repeat string))

(defcustom yaham-ffap-show-function 'find-file-other-window
  "*The function used to show another file by \\[yaham-ffap].
It must take one argument, the file name, create or reuse a buffer visiting
that file, and select the buffer in a window."
  :group 'yaham
  :type '(radio
          (function-item :doc "Show in same window" find-file)
          (function-item :doc "Show in another window" find-file-other-window)
          (function-item :doc "Show in another frame" find-file-other-frame)
          (function :tag "Your own function")))

(defcustom yaham-basic-indent 4
  "*How many spaces to indent new syntactic constructs."
  :group 'yaham
  :type 'integer)

(defcustom yaham-space-electric-flag t
  "*If non-nil, a space a after the first token on a line in Yaham mode reindents."
  :group 'yaham
  :type 'boolean)

(defcustom yaham-parens-electric-flag nil
  "*If non-nil, left parenthesis and left bracket insert matched pairs.
This means a skeleton inserting command runs instead of `self-insert-command'."
  :group 'yaham
  :require 'yaham-skel
  :type 'boolean)


(defconst yaham-syntax-table
  (let ((tbl (make-syntax-table)))
    (modify-syntax-entry ?\{ "(}1"    tbl)
    (modify-syntax-entry ?\} "){4"    tbl)
    (modify-syntax-entry ?-  ". 23n"  tbl)
    (modify-syntax-entry ?`  "'"      tbl)
    (modify-syntax-entry ?\' "_"      tbl)
    (modify-syntax-entry ?_  "_"      tbl)
    (modify-syntax-entry ?,  "."      tbl)
    (modify-syntax-entry ?\; "."      tbl)
    tbl)
  "Character syntax table to use in Yaham buffers.")

(defconst yaham-alt-syntax-table
  (let ((tbl (copy-syntax-table yaham-syntax-table)))
    (modify-syntax-entry ?\' "w" tbl)
    (modify-syntax-entry ?_  "w" tbl)
    tbl)
  "Alternate syntax table to use internally in Yaham buffers.")

(defconst yaham-font-lock-keywords
  `((,(eval-when-compile
        (regexp-opt
         '("case"
           "class"
           "data"
           "default"
           "deriving"
           "else"
           "foreign"
           "if"
           "import"
           "in"
           "infix"
           "infixl"
           "infixr"
           "instance"
           "let"
           "module"
           "newtype"
           "of"
           "then"
           "type"
           "where")
         'words))
     . font-lock-keyword-face)
    ("\\<\\(error\\|fail\\)\\>" . font-lock-warning-face)
    ("\\<_\\>" . font-lock-variable-name-face)
    ("\\((,*)\\|\\[\\]\\|\\<\\(True\\|False\\)\\>\\)" . font-lock-constant-face)
    ("\\<\\(catch\\|do\\|return\\)\\>" . font-lock-builtin-face)
    ("^\\([a-z_][A-Za-z0-9_']*\\)\\s *::" 1 font-lock-function-name-face)
    ("^(\\s *\\([-!#$%&*+./<=>?@\\^|~]+\\)\\s *)\\s *::" 1 font-lock-function-name-face)
    ("\\<[[:upper:]][A-Za-z0-9_']*\\>" . font-lock-type-face))
  "Keywords to fontify in Yaham buffers.")

(defsubst yaham-dashes-syntax (syn)
  (save-excursion
    (goto-char (match-beginning 1))
    (if (or
         (bolp)
         (not
          (memq
           (char-before)
           (eval-when-compile (string-to-list "-{!#$%&*+./<=>?@\\^|~")))))
        syn)))

(defsubst yaham-dot-syntax ()
  (let ((case-fold-search nil))
    (save-excursion
      (goto-char (match-beginning 1))
      (skip-chars-backward "A-Za-z0-9_'")
      (skip-chars-forward "0-9")
      (if (looking-at "[A-Z]")
          (eval-when-compile (string-to-syntax "_"))))))

(defconst yaham-font-lock-syntactic-keywords
  `((,(eval-when-compile
        (concat
         "\\(^\\|\\Sw\\)"
         "\\('\\)"
         "\\([^\\']\\|\\\\\\([abfnrtv'\"\\&]\\|[0-9]+\\|"
         "x[0-9a-fA-F]+\\|"
         "o[0-7]+\\|"
         "\\^[][A-Z@\\^_]\\|"
         "\\^\\(NUL\\|SOH\\|[SE]TX\\|EOT\\|ENQ\\|ACK\\|BEL\\|BS\\|HT\\|[LF]F\\|VT\\|CR\\|S[OI]\\|DLE\\|"
         "DC[1-4]\\|NAK\\|SYN\\|ETB\\|CAN\\|EM\\|SUB\\|ESC\\|[FGRU]S\\|SP\\|DEL\\)\\)\\)"
         "\\('\\)"))
     (2 "|") (6 "|"))
    ("\\(\\\\\\)[^\\\\\"]" (1 "."))     ;backslash is only an escape when it precedes another backslash or a dquote
    ("\\(\\.\\)\\([A-Za-z_]\\|[-!#$%&*+./<=>?@\\^|~]\\)"
     (1 (yaham-dot-syntax)))
    ("\\(-\\)-+\\([^-!#$%&*+./<=>?@\\^|~\n].*\\)?\\(\n\\)"
     (1 (yaham-dashes-syntax "<"))
     (3 (yaham-dashes-syntax ">"))))
  "Syntactic keywords to \"fontify\" in Yaham buffers.")

(defconst yaham-defun-start-keywords
  (eval-when-compile
    (concat
     "^\\(\\([a-z_][A-Za-z0-9_']*\\)\\s *::\\|"
     "(\\s *\\([-!#$%&*+./<=>?@\\^|~]+\\)\\s *)\\s *::\\|"
     (regexp-opt
      '("instance"
        "class"
        "type"
        "data"
        "newtype")
      'words)
     "\\)"))
  "Regexp matching keywords that start Haskell declarations if found at beginning of line.")

(defconst yaham-imenu-regexps
  '(("Instances" "^instance\\s +\\(.*[^-!#$%&*+./<=>?@\\^|~]=>\\s *\\)?\\(\\([[:upper:]][A-Za-z0-9_']*\\.\\)*[[:upper:]][A-Za-z0-9_']*\\)" 2)
    ("Classes" "^class\\s +\\(.*[^-!#$%&*+./<=>?@\\^|~]=>\\s *\\)?\\([[:upper:]][A-Za-z0-9_']*\\)" 2)
    ("Types" "^type\\s +\\([[:upper:]][A-Za-z0-9_']*\\)" 1)
    ("Data" "^\\(data\\|newtype\\)\\s +\\(.*[^-!#$%&*+./<=>?@\\^|~]=>\\s *\\)?\\([[:upper:]][A-Za-z0-9_']*\\)" 3)
    ("Infixes" "^(\\s *\\([-!#$%&*+./<=>?@\\^|~]+\\)\\s *)\\s *::" 1)
    ("Variables" "^\\([a-z_][A-Za-z0-9_']*\\)\\s *::" 1)))

(defconst yaham-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap "\C-c\C-g" 'yaham-toggle-gaps)
    (define-key kmap "\C-c\C-t" 'yaham-toggle-literate)
    (define-key kmap "\C-c\C-v" 'yaham-ffap)
    (define-key kmap "\C-c{"    'yaham-toggle-nested-comments)
    (define-key kmap "\C-c;"    'yaham-toggle-skeletons-layout)
    (define-key kmap [ M-left ] 'yaham-backward-into-nomenclature)
    (define-key kmap [ M-right ] 'yaham-forward-into-nomenclature)
    (define-key kmap "\M-i"     'yaham-indent-line)
    (define-key kmap "\M-j"     'yaham-indent-new-comment-line)
    (define-key kmap "\M-n"     'yaham-indent-forward-dwim)
    (define-key kmap "\M-p"     'yaham-indent-backward-dwim)
    (define-key kmap "\M-s"     'yaham-unindent-line)
    (define-key kmap " "        'yaham-electric-space)
    (define-key kmap "("        'yaham-electric-paren)
    (define-key kmap "["        'yaham-electric-paren)
    (define-key kmap [?\C-c mouse-2] 'yaham-ffap-at-mouse)
    (define-key kmap [?\C-c mouse-3] 'yaham-indent-as-mouse-line)
    (define-key kmap "\C-c1" (make-sparse-keymap))
    (define-key kmap "\C-c2" (make-sparse-keymap))
    kmap)
  "Keymap to use in Yaham buffers.")

(easy-menu-define yaham-mode-menu yaham-mode-map "Menu to use in Yaham buffers."
  '("Yaham"
    ["Find File at Point" yaham-ffap :help "Find the file suggested by the context around point"]
    ["Toggle Gaps" yaham-toggle-gaps :help "Toggle the treatment of newlines inside strings when filling"]
    ["Toggle Nested Comments" yaham-toggle-nested-comments :help "Toggle the preference of nested or line comments"]
    ["Toggle Skeletons Layout" yaham-toggle-skeletons-layout :help "Toggle the preference of layout or braced syntax for skeletons"]
    ["Toggle Literate View" yaham-toggle-literate :help "Toggle view of literate Haskell source between LaTeX and Yaham mode"]
    ["--" nil]
    ("Skeletons")))

(defconst yaham-abbrev-table (make-abbrev-table)
  "Abbreviation table to use in Yaham mode buffers.")
(add-to-list 'abbrev-table-name-list 'yaham-abbrev-table)

(defvar yaham-nested-comments-flag nil
  "When true, the mode will prefer nested comments over line comments.
Don't change this variable manually.  Use the command \\[yaham-toggle-nested-comments].")
(make-variable-buffer-local 'yaham-nested-comments-flag)

(defconst yaham-toggle-nested-comments-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [mode-line mouse-1]
      'yaham-toggle-nested-comments)
    kmap))

(defvar yaham-string-gaps-flag t
  "Determines whether or not to insert Haskell string \"gaps\" while filling.
Don't change this variable manually.  Use the command \\[yaham-toggle-gaps].")
(make-variable-buffer-local 'yaham-string-gaps-flag)

(defconst yaham-toggle-gaps-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [mode-line mouse-1]
      'yaham-toggle-gaps)
    kmap))

(defvar yaham-skeletons-layout-flag t
  "If non-nil, skeleton inserting commands prefer layout syntax.
Don't change this variable manually.  Use the command
\\[yaham-toggle-skeletons-layout].")
(make-variable-buffer-local 'yaham-skeletons-layout-flag)

(defconst yaham-toggle-skeletons-layout-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [mode-line mouse-1]
      'yaham-toggle-skeletons-layout)
    kmap))

(defsubst yaham-region-visible-p ()
  "Decide if we should act on the region."
  (and mark-active
       (/= (point) (mark))
       (or transient-mark-mode
           (memq last-command
                 '(mouse-drag-region mouse-save-then-kill
                   mark-word mark-sexp)))))

(put 'yaham-symbol 'forward-op 'yaham-forward-symbol)

(defun yaham-forward-symbol (n)
  (let ((case-fold-search nil))
    (with-syntax-table yaham-alt-syntax-table
      (cond
       ((< 0 n)
        (re-search-forward
         "\\([A-Za-z0-9_']\\>\\|[-!#$%&*+./<=>?@\\^|~]\\(\\([^-!#$%&*+./<=>?@\\^|~]\\)\\|\\'\\)\\)"
         nil 'move n)
        (if (match-end 3) (backward-char)))
       ((< n 0)
        (re-search-backward
         "\\(\\<[A-Za-z_]\\|\\(\\`\\|\\([^-!#$%&*+./<=>?@\\^|~]\\)\\)[-!#$%&*+./<=>?@\\^|~]\\)"
         nil 'move (- n))
        (if (match-beginning 3) (forward-char)))
       (t nil)))))

(defsubst yaham-symbol-at-point ()
  (let ((b (bounds-of-thing-at-point 'yaham-symbol)))
    (if b (buffer-substring-no-properties (car b) (cdr b)))))

;; decide if the point is on a module path in an import declaration
(defsubst yaham-in-import-modpath-p ()
  (let ((case-fold-search nil))
    (with-syntax-table yaham-alt-syntax-table
      (looking-back
       "\\<import\\s +\\(qualified\\s +\\)?\\(\\([A-Z][a-zA-Z0-9_']*\\.\\)*[A-Z][a-zA-Z0-9_']*\\)?"))))

(put 'yaham-modpath 'beginning-op 'yaham-beginning-of-modpath)
 
(defun yaham-beginning-of-modpath ()
  (let ((case-fold-search nil))
    (or (eobp) (forward-char 1))
    (with-syntax-table yaham-alt-syntax-table
      (catch 'found
        (while (re-search-backward "\\(\\`\\|[^.]\\)\\<[A-Z]" nil t)
          (goto-char (match-end 1))
          (if (or (yaham-in-import-modpath-p)
                  (looking-at "[A-Z][a-zA-Z0-9_']*\\.")) (throw 'found (point))))))))

(put 'yaham-modpath 'end-op 'yaham-end-of-modpath)

(defun yaham-end-of-modpath ()
  (let ((case-fold-search nil))
    (or (bobp) (backward-char 1))
    (with-syntax-table yaham-alt-syntax-table
      (catch 'found
        (while (re-search-forward "\\(\\`\\|[^.]\\)\\<[A-Z]" nil t)
          (goto-char (match-end 1))
          (cond
           ((yaham-in-import-modpath-p) 
            (skip-chars-forward "[A-Za-z0-9_'.]")
            (throw 'found (point)))
           ((looking-at "\\([A-Z][a-zA-Z0-9_']*\\.\\)+")
            (goto-char (1- (match-end 0)))
            (throw 'found (point)))
           (t nil)))))))


(defvar yaham-syntax-vector nil
  "A vector of string and comment intervals in current buffer.
It's always rebuilt by commands that need it and bound locally
since that is quick enough and avoids cache consistency headaches.")

(defsubst yaham-list->vector (l)
  "Convert a list to a vector, reversing the order."
  (let* ((len (length l))
         (v (make-vector len nil)))
    (let ((head l))
      (while head
        (setq len (1- len))
        (aset v len (car head))
        (pop head)))
    v))        

(defun yaham-bsearch-pos (pt vec)
  "Finds and returns the smallest I such that PT <= (aref (aref VEC I) 1).
If there is no such I, returns nil.  VEC must be a sorted vector whose
members are themselves vectors of the form [START END], representing
pairwise disjoint intervals."
  (let ((max (length vec)))
    (if (or (= max 0) (not (<= pt (aref (aref vec (1- max)) 1)))) nil
      (let ((min 0))
        (while (< min max)
          (let ((mid (/ (+ min max) 2)))
            (if (<= pt (aref (aref vec mid) 1))
                (setq max mid)
              (setq min (1+ mid)))))
        min))))

(defun yaham-build-syntax-vector ()
  "Build and return a vector of string and comment intervals in current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((state nil) (syntax-alist nil) (maxp (point-max)))
      (while (and (not (eobp))
                  (progn
                    (setq state (parse-partial-sexp
                                 (point) maxp nil nil state 'syntax-table))
                    (or (nth 3 state) (nth 4 state))))
        (let ((beg (nth 8 state)))
          (setq state (parse-partial-sexp
                       (point) maxp nil nil state 'syntax-table))
          (push (vector beg (point)) syntax-alist)))
      (yaham-list->vector syntax-alist))))

(defun yaham-in-syntax-p (start end)
  "Given `yaham-syntax-vector' syntax intervals, see if one contains region."
  (let ((i (yaham-bsearch-pos end yaham-syntax-vector)))
    (when i
      (let* ((interval (aref yaham-syntax-vector i))
             (istart (aref interval 0))
             (iend (aref interval 1)))
        (when (< istart start) interval)))))

(defsubst yaham-out-of-syntax (start end direction)
  (let ((i (yaham-in-syntax-p start end)))
    (if (not i) nil
      (goto-char (aref i direction))
      t)))


(defun yaham-beginning-of-defun-internal ()
  (let ((case-fold-search nil))
    (with-syntax-table yaham-alt-syntax-table
      (while
          (progn
            (re-search-backward yaham-defun-start-keywords (point-min) 'move)
            (and (not (bobp))
                 (yaham-out-of-syntax (match-beginning 1) (match-end 1) 0))))
      (looking-at yaham-defun-start-keywords))))

(defun yaham-beginning-of-defun ()
  "Move to the beginning of the top-level declaration at or before point."
  (interactive)
  (let ((yaham-syntax-vector (yaham-build-syntax-vector)))
    (yaham-beginning-of-defun-internal)))

(defun yaham-end-of-defun-internal ()
  (let ((case-fold-search nil))
    (with-syntax-table yaham-alt-syntax-table
      (when (and (looking-at yaham-defun-start-keywords)
                 (not (yaham-in-syntax-p (match-beginning 1) (match-end 1))))
        (goto-char (match-end 1)))
      (while
          (progn
            (re-search-forward yaham-defun-start-keywords (point-max) 'move)
            (and (not (eobp))
                 (yaham-out-of-syntax (match-beginning 1) (match-end 1) 1))))))
  (beginning-of-line)
  t)

(defun yaham-end-of-defun ()
  "Move to the end of the top-level declaration at or after point."
  (interactive)
  (let ((yaham-syntax-vector (yaham-build-syntax-vector)))
    (yaham-end-of-defun-internal)))

(defun yaham-current-defun ()
  "Return the name of the top-level declaration at point, or nil.
This is used for labelling changelog entries with \\[add-log]."
  (let ((case-fold-search nil))
    (with-syntax-table yaham-alt-syntax-table
      (save-excursion
        (let ((yaham-syntax-vector (yaham-build-syntax-vector)))
          (if (or (not (looking-at yaham-defun-start-keywords))
                  (yaham-in-syntax-p (match-beginning 1) (match-end 1)))
              (yaham-beginning-of-defun-internal)))
        (catch 'found
          (mapc
           (lambda (el)
             (let ((regexp (nth 1 el)) (subexp (nth 2 el)))
               (when (looking-at regexp)
                 (throw 'found (match-string-no-properties subexp)))))
           yaham-imenu-regexps)
          nil)))))


(defvar yaham-ffap-path-function nil
  "Function taking no arguments and returning a list of directories.
`yaham-ffap' looks for modules in these directories.")
(make-variable-buffer-local 'yaham-ffap-path-function)

(defsubst yaham-modpath-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'yaham-modpath)))
    (if bounds (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun yaham-this-module ()
  "Return the name of the module defined in the current buffer, if any."
  (let ((yaham-syntax-vector (yaham-build-syntax-vector)))
    (save-excursion
      (catch 'found
        (goto-char (point-min))
        (while (with-syntax-table yaham-alt-syntax-table
                 (re-search-forward
                  "^module\\s +\\([[:upper:]][A-Za-z0-9_']*\\(\\.[[:upper:]][A-Za-z0-9_']*\\)*\\)\\>"
                  nil 'move))
          (let ((start (match-beginning 1)) (end (match-end 1)))
            (unless (yaham-in-syntax-p start end)
              (throw 'found (buffer-substring-no-properties start end)))))
        nil))))

(defun yaham-default-filename ()
  (catch 'found
    (let ((mp (yaham-modpath-at-point)))
      (if mp
          (let ((fp (copy-sequence mp)) (i 0) (l (length mp)))
            (while (< i l)
              (if (= (aref fp i) ?\.  ) (aset fp i ?/  ))
              (setq i (1+ i)))
            (setq fp (concat fp yaham-ffap-suffix))
            (mapc
             (lambda (d)
               (let ((fullname (expand-file-name fp d)))
                 (if (file-exists-p fullname) (throw 'found fullname))))
             (append
              (list ".") yaham-ffap-path
              (if yaham-ffap-path-function (funcall yaham-ffap-path-function))))))
      (buffer-file-name))))

(defsubst yaham-read-file (prompt)
  (let ((default (yaham-default-filename)))
    (if (and default (not current-prefix-arg)) default
      (let ((fn (read-file-name prompt nil default t)))
        (if (file-name-absolute-p fn) fn
          (expand-file-name fn default-directory))))))

(defun yaham-find-file-and-jump (filename &optional sym)
  "Find the file FILENAME and maybe jump to SYM using imenu.
Ignore errors while jumping."
    (funcall yaham-ffap-show-function filename)
    (when sym
      (condition-case nil
          (let* ((index (assoc sym (imenu--make-index-alist)))
                 (pos (cdr index)))
            (when (and (number-or-marker-p pos) (> pos 0)) (imenu index)))
        (error nil))))

(defun yaham-ffap (filename)
  "Find the file suggested by the context around point."
  (interactive (list (yaham-read-file "Find file: ")))
  (let* ((case-fold-search nil)
         (sym
          (save-excursion
            (end-of-thing 'yaham-modpath)
            (if (looking-at "\\.\\(\\([a-z_][a-zA-Z0-9_']*\\)\\|(\\([-!#$%&*+./<=>?@\\^|~]+\\))\\)")
                (or (match-string-no-properties 2) (match-string-no-properties 3))))))
    (yaham-find-file-and-jump filename sym)))

(defun yaham-ffap-at-mouse (ev)
  "Find the file suggested by the context where mouse was clicked."
  (interactive "@e")
  (mouse-set-point ev)
  (call-interactively 'yaham-ffap))


;; these are copied from mindent to avoid dependency
(defun yaham-backward-to-same-indent (n)
  "Move point back  N lines with less or same indentation."
  (interactive "p")
  (beginning-of-line 1)
  (if (< n 0) (yaham-forward-to-same-indent (- n))
    (let ((yaham-syntax-vector (yaham-build-syntax-vector)))
      (while (> n 0)
        (let ((i (current-indentation)))
          (forward-line -1)
          (while (or (> (current-indentation) i)
                     (yaham-in-syntax-p (point) (1+ (point)))
                     (looking-at
                      (concat "[ \t]*\\(\n\\|" comment-start-skip "\\)")))
            (forward-line -1)))
        (setq n (1- n)))))
  (back-to-indentation))

(defun yaham-forward-to-same-indent (n)
  "Move point forward N lines with less or same indentation."
  (interactive "p")
  (beginning-of-line 1)
  (if (< n 0) (yaham-backward-to-same-indent (- n))
    (let ((yaham-syntax-vector (yaham-build-syntax-vector)))
      (while (> n 0)
        (let ((i (current-indentation)))
          (forward-line 1)
          (while (or (> (current-indentation) i)
                     (yaham-in-syntax-p (point) (1+ (point)))
                     (looking-at
                      (concat "[ \t]*\\(\n\\|" comment-start-skip "\\)")))
            (forward-line 1)))
        (setq n (1- n)))))
  (back-to-indentation))

(defun yaham-backward-to-less-indent (n)
  "Move point back  N lines with stricly less indentation."
  (interactive "p")
  (beginning-of-line 1)
  (if (< n 0) (yaham-forward-to-less-indent (- n))
    (let ((yaham-syntax-vector (yaham-build-syntax-vector)))
      (while (> n 0)
        (let ((i (current-indentation)))
          (forward-line -1)
          (while (or (>= (current-indentation) i)
                     (yaham-in-syntax-p (point) (1+ (point)))
                     (looking-at
                      (concat "[ \t]*\\(\n\\|" comment-start-skip "\\)")))
            (if (bobp) (error "Beginning of buffer"))
            (forward-line -1)))
        (setq n (1- n)))))
  (back-to-indentation))

(defun yaham-forward-to-less-indent (n)
  "Move point forward N lines with strictly less indentation."
  (interactive "p")
  (beginning-of-line 1)
  (if (< n 0) (yaham-backward-to-less-indent (- n))
    (let ((yaham-syntax-vector (yaham-build-syntax-vector)))
      (while (> n 0)
        (let ((i (current-indentation)))
          (forward-line 1)
          (while (or (>= (current-indentation) i)
                     (yaham-in-syntax-p (point) (1+ (point)))
                     (looking-at
                      (concat "[ \t]*\\(\n\\|" comment-start-skip "\\)")))
            (if (eobp) (error "End of buffer"))
            (forward-line 1)))
        (setq n (1- n)))))
  (back-to-indentation))

(defun yaham-indent-forward-dwim (&optional arg)
  "Move point forward to a line with indentation similar to current line.
The precise semantics of this command depend on the optional ARG,
and on the variable `last-command'.  If ARG is nil, an integer, or the
symbol `-' *and* `last-command' is not `yaham-forward-to-less-indent',
this command behaves like `yaham-forward-to-same-indent'.  Otherwise
it acts like `yaham-forward-to-less-indent' with an argument that is
the number of times the universal prefix argument was given."
  (interactive "P")
  (if (and (not (eq last-command 'yaham-forward-to-less-indent))
           (or (null arg)
               (integerp arg)
               (and (symbolp arg) (eq arg '-))))
      (yaham-forward-to-same-indent (prefix-numeric-value arg))
    (let ((real-arg
           (if (null arg) 1
             (truncate (log (prefix-numeric-value arg) 4)))))
      (yaham-forward-to-less-indent real-arg)
      (setq this-command 'yaham-forward-to-less-indent))))

(defun yaham-indent-backward-dwim (&optional arg)
  "Move point backward to a line with indentation similar to current line.
The precise semantics of this command depend on the optional ARG,
and on the variable `last-command'.  If ARG is nil, an integer, or the
symbol `-' *and* `last-command' is not `yaham-backward-to-less-indent',
this command behaves like `yaham-backward-to-same-indent'.  Otherwise
it acts like `yaham-backward-to-less-indent' with an argument that is
the number of times the universal prefix argument was given."
  (interactive "P")
  (if (and (not (eq last-command 'yaham-backward-to-less-indent))
           (or (null arg)
               (integerp arg)
               (and (symbolp arg) (eq arg '-))))
      (yaham-backward-to-same-indent (prefix-numeric-value arg))
    (let ((real-arg
           (if (null arg) 1
             (truncate (log (prefix-numeric-value arg) 4)))))
      (yaham-backward-to-less-indent real-arg)
      (setq this-command 'yaham-backward-to-less-indent))))

(defvar yaham-master-unique-id 0
  "Sequence number for unique symbols used for master-slave indentation.")

(defsubst yaham-master-gensym ()
  (prog1 (make-symbol (format "yaham-master-%d" yaham-master-unique-id))
    (setq yaham-master-unique-id (1+ yaham-master-unique-id))))

(defsubst yaham-unlink-from-master (p m-prop)
  (remove-text-properties p (1+ p) '(yaham-master-prop nil))
  (let ((m (car m-prop)) (prop (cdr m-prop)))
    (if (and m prop) (remove-text-properties m (1+ m) (list prop nil)))))

(defsubst yaham-link-to-master (p m n)
  (let ((prop (yaham-master-gensym)))
    (put-text-property m (1+ m) prop n)
    (put-text-property p (1+ p) 'yaham-master-prop prop)))

(defvar yaham-master-search-limit 4000
  "How many character positions around point to search for indentation master.")

(defun yaham-search-master (p)
  "Look for the master to which line containing P was indented.
If found, return a pair (M PROP) where M is the position of the master
and PROP is the text property at position M whose value is the
relative indentation, in units of `yaham-basic-indent'.

If P has been linked to a master but the master cannot be found
within `yaham-master-search-limit', return just (nil PROP)."
  (let ((prop (get-text-property p 'yaham-master-prop))
        (b (current-buffer)))
    (if (not prop) (cons nil nil)
      (let* ((pre-limit (- p yaham-master-search-limit))
             (limit (if (< (point-min) pre-limit) pre-limit nil))
             (m (previous-single-property-change p prop b limit)))
        (when (eq m limit)
          (setq pre-limit (+ p yaham-master-search-limit))
          (setq limit (if (< pre-limit (point-max)) pre-limit nil))
          (setq m (next-single-property-change p prop b limit)))
        (if (eq m limit) (cons nil prop)
          (if (< m p) (setq m (1- m)))
          (cons m prop))))))
                                                

(defun yaham-indent-relative (n &optional m)
  "Indent current line relative to some other line.
The line used as reference is the one containing position M;
If M is nil the previous non-blank line is used.
N is how many extra `yaham-basic-indent' units to indent."
  (unless m
    (setq m (save-excursion
              (beginning-of-line)
              (skip-syntax-backward " ")
              (point))))
  (let ((cur (current-indentation))
        (goal (save-excursion
                (goto-char m)
                (+ (current-indentation) (* n yaham-basic-indent)))))
    (if (< goal 0) (setq goal 0))
    ;; link the lines with a property for indentation functions
    (save-excursion
      (back-to-indentation)
      (let ((m-prop (yaham-search-master (point))))
        (yaham-unlink-from-master (point) m-prop))
      (yaham-link-to-master (point) m n))
    (if (/= goal cur)
        (if (<= (current-column) cur) (indent-line-to goal)
          (save-excursion (indent-line-to goal))))))

(defun yaham-indent-line (n)
  "Indent current line N more levels than previous nonblank line."
  (interactive "*p")
  (yaham-indent-relative n nil))

(defun yaham-unindent-line (n &optional nomsg)
  "Unindent current line N levels.
That is, find the Nth stricly less indented line preceding this one and
reindent to it.  If N is negative, reindent to the -Nth stricly less
indented following line, instead.  As a special case, this command will
not change the buffer contents if the argument is 0.  In a program,
second argument NOMSG non-nil means suppress the echoing of the matching
previous line."
  (interactive "*p\ni")
  (if (/= n 0)
      (let* ((goal-point (save-excursion
                           (yaham-backward-to-less-indent n)
                           (point)))
             (text (save-excursion
                     (goto-char goal-point)
                     (concat
                      (if (bolp) "" "...")
                      (buffer-substring (point) (progn (end-of-line) (point)))))))
        (yaham-indent-relative 0 goal-point)
        (unless nomsg (message (format "Matches %s" text))))))

(defun yaham-indent-as-mouse-line (n e)
  "Indent current line relative to the line where mouse was clicked.
N is how many extra `yaham-basic-indent' units to indent."
  (interactive "*@P\ne")
  (let ((nn (if n (prefix-numeric-value n) 0))
        (m (save-excursion (mouse-set-point e) (point))))
    (yaham-indent-relative nn m)))

;; ripped from cc-mode
(defun yaham-forward-into-nomenclature (arg)
  "Move forward to end of a nomenclature section or word.
With \\[universal-argument] (programmatically, argument ARG),
do it that many times.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (let ((case-fold-search nil))
    (if (> arg 0)
        (with-syntax-table yaham-alt-syntax-table
          (re-search-forward
           "\\(\\W\\|[_']\\)*\\([A-Z]*[a-z0-9]*\\)"
           (point-max) t arg))
      (while (and (< arg 0)
                  (with-syntax-table yaham-alt-syntax-table
                    (re-search-backward
                     "\\(\\W\\|[a-z0-9]\\)[A-Z]+\\|\\(\\W\\|[_']\\)\\w+"
                     (point-min) 0)))
	(forward-char 1)
	(setq arg (1+ arg))))))

(defun yaham-backward-into-nomenclature (arg)
  "Move backward to beginning of a nomenclature section or word.
With ARG, move that many times.  If ARG is negative, move
forward.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (yaham-forward-into-nomenclature (- arg)))


(defun yaham-imenu-sublist (regexp subexp)
  (let ((sublist nil) (done nil) (case-fold-search imenu-case-fold-search))
    (save-excursion
      (goto-char (point-max))
      (with-syntax-table yaham-alt-syntax-table
        (while (not done)
          (beginning-of-line)
          (cond
           ((not (re-search-backward regexp nil t))
            (setq done t))
           ((not (yaham-out-of-syntax (match-beginning 0) (match-end 0) 0))
            (push (cons (match-string-no-properties subexp)
                        (copy-marker (match-beginning subexp))) sublist))
           (t nil)))))
    sublist))

(defun yaham-imenu-create-index ()
  (let ((index nil) (toplist nil)
        (yaham-syntax-vector (yaham-build-syntax-vector)))
    (mapc
     (lambda (el)
       (let ((name (nth 0 el)) (regexp (nth 1 el)) (subexp (nth 2 el)))
         (let ((sublist (yaham-imenu-sublist regexp subexp)))
           (unless (null sublist)
             (push (cons name sublist) index)
             (setq toplist (append sublist toplist))))))
     yaham-imenu-regexps)
    (append index (sort toplist 'imenu--sort-by-position))))

(defun yaham-in-lone-string-p ()
  "Returns non-nil if and only if point is in a lone string.
A lone string is one which isn't preceded by any code on the line
it starts on, and isn't followed by any code on the line it ends on.
This is the only kind of string `yaham-fill-syntax' will reformat.
The actual value returned is a pair of positions (START . END) that
delimits the contents of the string, not including the string
starter or ender or any surrounding space."
  (let ((interval (yaham-in-syntax-p (point) (1+ (point)))))
    (save-excursion
      (when (and interval
                 (goto-char (aref interval 0))
                 (looking-at "\"")
                 (looking-back "^[ \t]*")
                 (goto-char (aref interval 1))
                 (looking-at "[ \t]*$"))
        (let* ((start
                (progn
                  (goto-char (aref interval 0))
                  (looking-at "\"[ \t]*")
                  (goto-char (match-end 0))
                  (point)))
               (end
                (progn
                  (goto-char (1- (aref interval 1)))
                  (skip-chars-backward " \t")
                  (max start (point)))))
          (cons start end))))))          

(defun yaham-add-gaps (start end)
  "Add a Haskell \"string gap\" to all lines ending within START and END."
  (save-excursion
    (goto-char start)
    (end-of-line 1)
    (while (and (not (eobp)) (<= (point) end))
      (insert " \\n\\")
      (beginning-of-line 2)
      (insert "\\")
      (end-of-line))))

(defun yaham-remove-gaps (start end)
  "Remove Haskell \"string gaps\" from all lines ending within START and END."
  (save-excursion
    (goto-char start)
    (end-of-line 1)
    (while (and (not (eobp)) (<= (point) end))
      (let ((p (point)))
        (beginning-of-line 2)
        (skip-chars-forward " \t")
        (when  (and (char-equal (char-before p) ?\\ )
                    (char-equal (char-before (1- p)) ?n)
                    (char-equal (char-before (- p 2)) ?\\ )
                    (char-equal (char-after) ?\\ ))
          (delete-region (- p 3) p)
          (delete-char 1)))
      (end-of-line))))

(defun yaham-in-lone-nested-comment-p ()
  "Returns non-nil if and only if point is in a lone nested comment.
A lone comment is one which isn't preceded by any code on the line
it starts on, and isn't followed by any code on the line it ends on.
This is the only kind of comment `yaham-fill-syntax' will reformat.
The actual value returned is a pair of positions (START . END) that
delimits the contents of the comment, not including the comment
starter or ender or any surrounding space."
  (let ((interval (yaham-in-syntax-p (point) (1+ (point)))))
    (save-excursion
      (when (and interval
                 (goto-char (aref interval 0))
                 (looking-at "{-")
                 (looking-back "^[ \t]*")
                 (goto-char (aref interval 1))
                 (looking-at "[ \t]*$"))
        (let* ((start
                (progn
                  (goto-char (aref interval 0))
                  (looking-at "{-\\(#\\|-*\\( [|#^]\\| \\*+\\)?\\)[\t ]*")
                  (goto-char (match-end 0))
                  (point)))
               (end
                (progn
                  (goto-char (1- (aref interval 1)))
                  (skip-chars-backward "-")
                  (if (char-equal ?#  (char-before))
                      (backward-char 1))
                  (skip-chars-backward " \t")
                  (max start (point)))))
          (cons start end))))))          

(defun yaham-in-lone-line-comment-p ()
  "Returns non-nil if and only if point is in a lone line comment.

A lone comment is one which isn't preceded by any code on the line
it starts on.

This is the only kind of comment `yaham-fill-syntax' will reformat.  The
actual value returned is a pair of positions (START . END) that delimits
the contents of the comment, including the comment starter and any
surrounding space."
  (let ((lone-regexp "^[ \t]*--+\\([^-!#$%&*+./<=>?@\\^|~\n].*\\)?\n")
        (last nil) (start nil) (end nil))
    (when (save-excursion
            (beginning-of-line)
            (looking-at lone-regexp))
      (setq start
            (save-excursion
              (beginning-of-line)
              (catch 'start
                (while (not (bobp))
                  (unless (looking-at lone-regexp)
                    (throw 'start last))
                  (setq last (point))
                  (forward-line -1))
                (unless (looking-at lone-regexp)
                  (throw 'start last))
                (point))))
      (when start
        (setq end
              (save-excursion
                (beginning-of-line 1)
                (catch 'end
                  (while (not (eobp))
                    (unless (looking-at lone-regexp)
                      (throw 'end (point)))
                    (forward-line 1))
                  (point))))
        (and end (cons start end))))))

(defsubst yaham-line-comment-starter (start)
  "Determine the comment starter string on the current line."
  (save-excursion
    (goto-char start)
    (looking-at (concat "^[ \t]*\\(" comment-start-skip "\\)"))
    (match-string-no-properties 1)))

(defun yaham-comment-filled-lines (start end level initial)
  "Turn a region of filled lines into line comments."
  (let ((len (length initial)))
    (save-excursion
      (goto-char start)
      (insert initial)
      (indent-line-to (- level len))
      (forward-line 1)
      (while (and (not (eobp)) (< (point) end))
        (insert (make-string (1- len) ?- ) " " )
        (indent-line-to (- level len))
        (forward-line 1)))))

(defun yaham-syntax-column (start)
  "Determine indentation of syntax (comment or string) starting at START."
  (save-excursion
    (goto-char start)
    (back-to-indentation)
    (let* ((extra-ind (get-text-property (point) 'yaham-indent-cache))
           (correction (if extra-ind (- extra-ind (current-indentation)) 0)))
      (looking-at
       (concat
        "\\("
        comment-start-skip
        "\\|\"[ \t]*"
        "\\)"))
      (goto-char (match-end 0))
      (+ correction (current-column)))))

(defun yaham-indent-syntax (start end)
  "Indent a region of either nested comments or a string literal."
  (let ((startcol (yaham-syntax-column start)))
    (save-excursion
      (goto-char start)
      (when (and (bolp) (not (eolp)))
        (indent-line-to startcol))
      (forward-line 1)
      (while (and (not (eobp)) (<= (point) end))
        (indent-line-to startcol)
        (forward-line 1)))))

(defun yaham-fill-syntax-internal (&optional just)
  (let (lone-bounds)
    (cond
     ((setq lone-bounds (yaham-in-lone-line-comment-p))
      (let* ((start (car lone-bounds))
             (level (yaham-syntax-column start))
             (initial (yaham-line-comment-starter start))
             (fill-column (- fill-column level))
             (end (copy-marker (cdr lone-bounds)))
             (comment-start "-- ")
             (comment-end "")
             (comment-continue nil))
        (unwind-protect
            (save-excursion
              (uncomment-region start end)
              (save-restriction
                (narrow-to-region start end)
                (fill-region (point-min) (point-max) just))
              (yaham-comment-filled-lines start end level initial))
          (set-marker end nil))))
     ((setq lone-bounds (yaham-in-lone-nested-comment-p))
      (let ((start (car lone-bounds)) (end (copy-marker (cdr lone-bounds))))
        (unwind-protect
            (save-excursion
              (goto-char start)
              (let* ((indent (current-column))
                     (fill-column (- fill-column indent)))
                (save-restriction
                  (narrow-to-region start end)
                  (fill-region (point-min) (point-max) just))
                (yaham-indent-syntax start end)))
          (set-marker end nil))))
     ((and
       (setq lone-bounds (yaham-in-lone-string-p))
       yaham-string-gaps-flag)
      (let ((start (car lone-bounds)) (end (copy-marker (cdr lone-bounds))))
        (unwind-protect
            (save-excursion
              (goto-char start)
              (let* ((indent (current-column))
                     (fill-column (- fill-column indent)))
                (save-restriction
                  (narrow-to-region start end)
                  (yaham-remove-gaps (point-min) (point-max))
                  (fill-region (point-min) (point-max) just)
                  (yaham-add-gaps (point-min) (point-max)))
                (yaham-indent-syntax start end)))
          (set-marker end nil))))
     (lone-bounds
      (let ((start (car lone-bounds)) (end (copy-marker (cdr lone-bounds))))
        (unwind-protect
            (save-excursion
              (goto-char start)
              (let* ((indent (current-column))
                     (fill-column (- fill-column indent)))
                (save-restriction
                  (narrow-to-region start end)
                  (fill-region (point-min) (point-max) just)
                  (yaham-indent-syntax start end))))
          (set-marker end nil))))
     (t nil)))
  t)

(defun yaham-fill-syntax (&optional just)
  "Like \\[fill-paragraph], but only for lone Haskell comments and strings.
See `yaham-indent-lone-nested-comment-p' for the definition of lone comments."
  (interactive "*P")
  (let ((yaham-syntax-vector (yaham-build-syntax-vector)))
    (yaham-fill-syntax-internal just)))

(defun yaham-indent-new-comment-line (&optional soft)
  "Break line at point and indent, continuing comment or string if within one."
  (interactive "*")
  (let ((yaham-syntax-vector (yaham-build-syntax-vector)))
    (cond
     ((yaham-in-lone-line-comment-p)
      (let* ((level (yaham-syntax-column (point)))
             (initial (yaham-line-comment-starter (point)))
             (len (length initial)))
        (delete-horizontal-space)
        (if soft (insert-and-inherit ?\n) (newline 1))
        (if fill-prefix
            (progn (indent-to-left-margin)
                   (insert-and-inherit fill-prefix))
          (insert (make-string (1- len) ?- ) " " )
          (indent-line-to (- level len))
          (forward-char len))))
     ((yaham-in-lone-nested-comment-p)
      (let ((startcol (yaham-syntax-column (point))))
        (delete-horizontal-space)
        (if soft (insert-and-inherit ?\n) (newline 1))
        (if (not fill-prefix) (indent-line-to startcol)
          (indent-to-left-margin)
          (insert-and-inherit fill-prefix))))
     ((yaham-in-lone-string-p)
      (let ((startcol (yaham-syntax-column (point))))
        (delete-horizontal-space)
        (when yaham-string-gaps-flag
          (insert-and-inherit "\\n\\")
          (save-excursion
            (skip-syntax-forward " ")
            (insert-and-inherit "\\")))
        (if soft (insert-and-inherit "\n")
          (newline 1))
        (if (not fill-prefix) (indent-line-to startcol)
          (indent-to-left-margin)
          (insert-and-inherit fill-prefix))))
     (t
      (delete-horizontal-space)
      (if soft (insert-and-inherit ?\n) (newline 1))
      (if fill-prefix
	  (progn
	    (indent-to-left-margin)
	    (insert-and-inherit fill-prefix))
        (indent-according-to-mode))))))


(defun yaham-toggle-nested-comments (&optional arg)
  "Toggle the preference of nested or line comments.

With ARG, turn nested comments on iff ARG is positive."
  (interactive "P")
  (prog1
      (setq yaham-nested-comments-flag 
            (if (null arg)
                (not yaham-nested-comments-flag)
              (> (prefix-numeric-value arg) 0)))
    (setq comment-start
          (if yaham-nested-comments-flag
              "{- "
            "-- "))
    (setq comment-end
          (if yaham-nested-comments-flag
              " -}"
            ""))
    (setq comment-continue (if yaham-nested-comments-flag "  "))
    (force-mode-line-update)))

(defun yaham-toggle-gaps (&optional arg)
  "Toggle the treatment of newlines inside strings when filling.

With ARG, turn gaps on iff ARG is positive."
  (interactive "P")
  (prog1
      (setq yaham-string-gaps-flag
            (if (null arg)
                (not yaham-string-gaps-flag)
              (> (prefix-numeric-value arg) 0)))
    (force-mode-line-update)))

(defun yaham-toggle-skeletons-layout (&optional arg)
  "Toggle preference for layout syntax when insrting skeletons.

With ARG, turn layout on iff ARG is positive."
  (interactive "P")
  (prog1
      (setq yaham-skeletons-layout-flag
            (if (null arg)
                (not yaham-skeletons-layout-flag)
              (> (prefix-numeric-value arg) 0)))
    (force-mode-line-update)))

(defun yaham-format-mode-line ()
  (when (and (listp mode-line-format)
             (not (null mode-line-format))
             (not (integerp (car mode-line-format)))
             (not (symbolp (car mode-line-format))))
    (let ((hd (butlast mode-line-format))
          (tl (car (last mode-line-format))))
      (let ((newtl
             (list '(:eval
                     (propertize
                      (if yaham-skeletons-layout-flag
                          " L" " ;")
                      'help-echo "mouse-1: toggle layout skeletons"
                      'local-map yaham-toggle-skeletons-layout-map))
                   '(:eval
                     (propertize
                      (if yaham-string-gaps-flag
                          " \\G\\" " \\n\\")
                      'help-echo "mouse-1: toggle string gaps"
                      'local-map yaham-toggle-gaps-map))
                   '(:eval
                     (propertize
                      (if yaham-nested-comments-flag
                          " {- -}" "  --  ")
                      'help-echo "mouse-1: toggle nested comments"
                      'face 'font-lock-comment-face
                      'local-map yaham-toggle-nested-comments-map))
                   tl)))
        (setq mode-line-format (append hd newtl))))))

(defun yaham-electric-paren (n)
  "Insert a matching pair skeleton if applicable, otherwise just self-insert.
If a skeleton is inserted, N is the number of slots in the tuple or list,
not just a repeat count."
  (interactive "*p")
  (setq this-command 'self-insert-command)
  (if (or (not yaham-parens-electric-flag)
          (looking-back "\\(\\s\\\\|\\s/\\|'\\)")
          (let ((yaham-syntax-vector (yaham-build-syntax-vector)))
            (yaham-in-syntax-p (point) (1+ (point)))))
      (self-insert-command n)
    (require 'yaham-skel)
    (funcall
     (lookup-key yaham-mode-map
                 (vector ?\C-c ?2 last-command-char)) n)))

(defun yaham-electric-space (n)
  "Reindent current line when typing space after the first token."
  (interactive "*p")
  (setq this-command 'self-insert-command)
  (self-insert-command n)
  (if (and yaham-space-electric-flag
           (looking-back "^[ \t]*\\S + "))
      (indent-according-to-mode)))

(defun yaham-find-tag-default ()
  "Value of `find-tag-default-function' in Yaham buffers."
  (with-syntax-table yaham-alt-syntax-table
    (word-at-point)))

;;;###autoload
(defun yaham-mode ()
  "Major mode for editing Haskell code.
\\<yaham-mode-map> Some useful commands are:

\\[yaham-ffap] - Find the file suggested by the context around point.
\\[yaham-toggle-gaps] - Toggle the treatment of newlines inside strings when filling.
\\[yaham-toggle-nested-comments] - Toggle the preference of nested or line comments.
\\[yaham-toggle-skeletons-layout] - Toggle preference for layout syntax when insrting skeletons.
\\[yaham-toggle-literate] - Toggle view of literate Haskell source between LaTeX and Yaham mode.
\\[yaham-forward-into-nomenclature] - Move forward to end of a nomenclature section or word.
\\[yaham-backward-into-nomenclature] - Move backward to beginning of a nomenclature section or word.
\\[yaham-indent-line] - Indent current line more.
\\[yaham-indent-new-comment-line] - Break line at point and indent, continuing comment or string if within one.
\\[yaham-indent-forward-dwim] - Move point forward to a line with indentation similar to current line.
\\[yaham-indent-backward-dwim] - Move point backward to a line with indentation similar to current line.
\\[yaham-unindent-line] - Unindent current line.
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map yaham-mode-map)
  (setq major-mode 'yaham-mode)
  (setq mode-name "Yaham")
  (set-syntax-table yaham-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start
        (if yaham-nested-comments-flag
            "{- "
          "-- "))
  (make-local-variable 'comment-end)
  (setq comment-end
        (if yaham-nested-comments-flag
            " -}"
          ""))
  (make-local-variable 'comment-continue)
  (setq comment-continue (if yaham-nested-comments-flag "  "))
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\({\\|\\s<\\)-\\(#\\|-*\\( [|#^]\\| \\*+\\)?\\)[\t ]*")
  (make-local-variable 'comment-use-syntax)
  (setq comment-use-syntax t)
  (make-local-variable 'beginning-of-defun-function)
  (setq beginning-of-defun-function 'yaham-beginning-of-defun)
  (make-local-variable 'end-of-defun-function)
  (setq end-of-defun-function 'yaham-end-of-defun)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(yaham-font-lock-keywords
          nil nil ((?_ . "w") (?' . "w"))
          beginning-of-defun
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-keywords
           . yaham-font-lock-syntactic-keywords)))
  (setq parse-sexp-lookup-properties t)
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'yaham-imenu-create-index)
  (imenu-add-menubar-index)
  (make-local-variable 'add-log-current-defun-function)
  (setq add-log-current-defun-function 'yaham-current-defun)
  (make-local-variable 'find-tag-default-function)
  (setq find-tag-default-function 'yaham-find-tag-default)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'yaham-tab-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'yaham-tab-region)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat page-delimiter "\\|[ \t]*$" ))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start paragraph-separate)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'yaham-fill-syntax)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because mode specific fill code should do the job.
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'comment-line-break-function)
  (setq comment-line-break-function 'yaham-indent-new-comment-line)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  ;; abbrev setup
  (setq local-abbrev-table yaham-abbrev-table)
  (make-local-variable 'yaham-string-gaps-flag)
  (make-local-variable 'yaham-nested-comments-flag)
  (yaham-format-mode-line)
  (run-mode-hooks 'yaham-mode-hook))

(defun yaham-narrow-to-code (mode)
  "Narrow to the literate code section at point and switch to MODE."
  (interactive "CMode: ")
  (let ((start
         (save-excursion
           (if (re-search-backward "^\\\\begin{code}.*\n" nil 'move)
               (goto-char (match-end 0)))
           (point)))
        (end
         (save-excursion
           (if (re-search-forward "^\\\\end{code}" nil 'move)
               (goto-char (match-beginning 0)))
           (point))))
    (narrow-to-region start end)
    (funcall mode)))

(defsubst yaham-widen-to-normal ()
  (widen)
  (normal-mode))

;;;###autoload
(defun yaham-toggle-literate (&optional arg)
  "Toggle view of literate Haskell source between Yaham and LaTeX mode.
Without argument it toggles between Yaham mode and the normal mode,
which will usually be some variant of `latex-mode' depending on
the filename and `auto-mode-alist'.  With a positive argument ARG it
always narrows and enters Yaham mode, with negative or zero ARG it
widens and enters the normal mode, and with ARG non-nil but not an integer
it narrows but prompts for the mode to enter."
  (interactive "P")
  (let ((narrowed (/= 1 (point-min))))
    (cond
     ((and (null arg) narrowed)
      (yaham-widen-to-normal))
     ((and (null arg) (not narrowed))
      (yaham-narrow-to-code 'yaham-mode))
     ((not (integerp arg))
      (call-interactively 'yaham-narrow-to-code))
     ((< 0 arg)
      (yaham-narrow-to-code 'yaham-mode))
     (t
      (yaham-widen-to-normal)))))

;;;###autoload
(defun yaham-create-tags-table (d &optional visit)
  "Write a TAGS file of all Haskell files in directory D.
If the optional flag VISIT is non-nil, visit the tags table when done.
Currently, this will not tag Bird type literate sources correctly,
and there are no plans to fix that."
  (interactive "DBuild tags table for directory: \nP")
  (let* ((tf (expand-file-name "TAGS" d))
         (b (find-file-noselect tf))
         (files (directory-files d t "\\.l?hs\\'")))
    (save-some-buffers nil (lambda () (member (buffer-file-name) files)))
    (with-current-buffer b
      (erase-buffer)
      (message "Building tags table in %s..." tf)
      (while files
        (let ((f (pop files)))
          (insert "\n" (file-name-nondirectory f) ",\n" )
          (let* ((start (point))
                 (bf (generate-new-buffer f)))
            (with-current-buffer bf
              (insert-file-contents f)
              (yaham-mode)
              (let ((l (yaham-imenu-create-index)))
                (while l
                  (let* ((item (pop l))
                         (offset (cdr item)))
                    (if (and (number-or-marker-p offset) (< 0 offset))
                        (let* ((oi (if (markerp offset) (marker-position offset) offset))
                               (name (car item))
                               (len (length name))
                               (line (count-lines (point-min) oi))
                               (prefix (progn
                                         (goto-char offset)
                                         (beginning-of-line)
                                         (buffer-substring-no-properties (point) (+ len oi)))))
                          (with-current-buffer b
                            (insert (format "%s%s%d,%d\n" prefix name line oi))))))))
              (kill-buffer nil))
            (let ((len (- (point) start)))
              (save-excursion
                (goto-char (1- start))
                (insert (int-to-string len)))))))
      (message "Building tags table in %s...done" tf)))
  (if visit (visit-tags-table d)))

(provide 'yaham)


;; Local Variables:
;; generated-autoload-file: "/home/itz/src/yaham/yaham-autoloads.el"
;; End:

;;; yaham.el ends here
