;;; yaham-skel.el --- skeleton (form) support for Yaham mode

;; Copyright (C) 2007 Ian Zimmerman <itz@madbat.mine.nu>

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the conditions spelled out in
;; the file LICENSE are met.

;; $Id: yaham-skel.el 89 2007-09-03 04:36:14Z itz $

(require 'easymenu)
(require 'yaham)

(defvar yaham-skel-insert-hook nil
  "*Normal hook run after skeleton insertion.
See `yaham-skel-process-spec' for more information.")

(defvar yaham-skel-args nil
  "Arguments passed to a skeleton inserting function.
See `yaham-skel-def' for more information.")

(defun yaham-skel-process-spec (spec &optional abbrev)
  "Insert a skeleton at point according to SPEC.
SPEC is a list with the following kinds of members:

* a string; it gets inserted at point.
* the symbol `:mark'; a mark at point is added to the mark ring.
* the symbol `:in'; the current line is auto-indented
  with `indent-according-to-mode'.
* the symbol `:addin'; the next member of SPEC must be an integer N.
  If N is negative, `yaham-unindent-line' is called with argument -N;
  otherwise `yaham-indent-line' with argument N.
* the symbol `:yank'; if, at the time this function is called,
  the mark is active and the region has been highlighted, the region
  substring is deleted and reinserted at point.  Otherwise, the behavior
  is the same as for `:mark'.
* the symbol `:copy'; this is just like `:yank' except that it doesn't
  delete the region substring from its present location.
* the symbol `:nl'; a newline is inserted at point conditionally
  (i.e. only if point is not at the beginning of a line).
* the symbol `:layout?' introduces a 2 way branch: if
  `yaham-skeletons-layout-flag' is non-nil the following item is
  processed normally and the one after that is skipped, but if the flag
  is nil the immediately following item is skipped.
* the symbol `:eval'; the next member of SPEC is treated as a Lisp form,
  it is evaluated and the value is consed with the rest of SPEC,
  making it the next item in SPEC to be examined.
* the symbol `:drop'; this is like `:eval' but the value is dropped
  instead, i.e. the following form is evaluated purely for side effects.
* an integer N; the next member of SPEC is replicated N times and
  prepended to the rest of SPEC, i.e. N works as a repeat count for
  the next member.
* nil; ignored.
* a list L; L is prepended to the rest of SPEC, i.e. \"spliced\" into
  the top level of SPEC.

The second, optional, argument ABBREV is used when this function is
called as a result of automatic abbrev expansion (see `abbrev-mode').
In this case, if the point is within a comment or a string, full
skeleton expansion is suppressed and only the keyword ABBREV is inserted.

Before returning run the normal hook `yaham-skel-insert-hook' with
a local narrowing to the text that has been inserted."
  (setq deactivate-mark t)
  (if (and abbrev
           (eq this-command 'self-insert-command)
           (let ((yaham-syntax-vector (yaham-build-syntax-vector)))
             (yaham-in-syntax-p (point) (1+ (point)))))
      (insert abbrev)
    (let ((bounds nil)
          (at-white (looking-at "\\s "))
          (p0 (copy-marker (point)))
          (p1 (copy-marker (point) t))
          first-mark)
      (when (yaham-region-visible-p)
        (setq bounds (cons (copy-marker (region-beginning) t)
                           (copy-marker (region-end)))))
      ;; must insert a space first to ensure a word boundary.
      ;; this is deleted at the end.
      (insert " ")
      (set-marker-insertion-type p1 nil)
      (unwind-protect
          (progn
            (while spec
              (let ((item (pop spec)))
                (cond
                 ((stringp item)
                  (insert item))
                 ((eq item :mark)
                  (push-mark (point) t)
                  (unless first-mark (setq first-mark (copy-marker (point)))))
                 ((eq item :addin)
                  (let ((n (pop spec)))
                    (unless (integerp n) (error "Invalid Yaham skeleton"))
                    (if (< n 0) (yaham-unindent-line (- n) t)
                      (yaham-indent-line n))))
                 ((eq item :in)
                  (indent-according-to-mode))
                 ((eq item :yank)
                  (if (not bounds) (push :mark spec)
                    (let ((str (buffer-substring (car bounds) (cdr bounds))))
                      (delete-region (car bounds) (cdr bounds))
                      ;; if this was adjacent to the space we inserted at the beginning,
                      ;; and there is more space to the left of it now, delete it now
                      (if (and (= (cdr bounds) p0)
                               (/= (car bounds) (cdr bounds))
                               (/= p0 p1)
                               (save-excursion
                                 (goto-char p0)
                                 (looking-back "[ \t]")))
                          (delete-region p0 p1))
                      (let ((p (point)))
                        (insert str)
                        (let ((p2 (point)))
                          (save-excursion
                            (goto-char p)
                            (if (search-forward "\n" p2 t)
                                (indent-region (point) p2 nil))))))))
                 ((eq item :copy)
                  (if (not bounds) (push-mark (point) t)
                    (let ((p (point)) (str (buffer-substring (car bounds) (cdr bounds))))
                      (insert str)
                      (let ((p2 (point)))
                        (save-excursion
                          (goto-char p)
                          (if (search-forward "\n" p2 t)
                              (indent-region (point) p2 nil)))))))
                 ((eq item :nl)
                  (unless (<= (current-column) (current-indentation))
                    (newline)))
                 ((eq item :layout?)
                  (let* ((iftrue (pop spec))
                         (iffalse (pop spec)))
                    (if yaham-skeletons-layout-flag
                        (push iftrue spec)
                      (push iffalse spec))))
                 ((eq item :eval)
                  (let ((expr (pop spec)))
                    (push (eval expr) spec)))
                 ((eq item :drop)
                  (let ((expr (pop spec)))
                    (eval expr)))
                 ((integerp item)
                  (let ((next (pop spec)))
                    (while (< 0 item)
                      (push next spec)
                      (setq item (1- item)))))
                 ((null item)
                  nil)
                 ((listp item)
                  (setq spec (append item spec)))
                 (t
                  (error "Invalid Yaham skeleton")))))
            (delete-region p0 p1)         ;deletes the initial space
            ;; if we were at whitespace to start with, but not now, insert one
            (when (and at-white (looking-at "\\S "))
              (insert " ")
              (backward-char))
            (if yaham-skel-insert-hook
                (save-restriction
                  (narrow-to-region p0 (point))
                  (run-hooks 'yaham-skel-insert-hook)))
            (when (and first-mark (/= (point) first-mark))
              (unless (= (point) (mark t)) (push-mark (point) t))
              (goto-char first-mark))
            (when abbrev (abbrev-symbol abbrev)))
        (set-marker p0 nil)
        (set-marker p1 nil)
        (when first-mark (set-marker first-mark nil))
        (when bounds
          (set-marker (car bounds) nil)
          (set-marker (cdr bounds) nil))))))

(defconst yaham-skel-blurb "\n\nPlease see `yaham-skel-process-spec' for more details on how this works,
and in particular how it interacts with the mark and region."
  "Blurb to include in docstrings for skeleton inserting commands.")

(defun yaham-skel-def (sym spec keys menuitem &optional abbrev)
  "Define an interactive command SYM inserting skeleton var SYM.
SPEC is the desired interactive spec string for SYM.
Add a binding invoking SYM by KEYS, a key sequence, to `yaham-mode-map'.
Add MENUITEM invoking SYM to `yaham-mode-menu'.
Optionally, add a definition running SYM via ABBREV to `yaham-abbrev-table'."
  (let* ((shortdoc (documentation-property sym 'variable-documentation))
         (help (substring shortdoc 0 (string-match "\n" shortdoc)))
         (doc (concat shortdoc yaham-skel-blurb)))                      
    (fset sym
          `(lambda (&rest yaham-skel-args)
             (interactive ,spec)
             (yaham-skel-process-spec ,sym ,abbrev)))
    (put sym 'function-documentation doc)
    (put sym 'no-self-insert t)
    (define-key yaham-mode-map keys sym)
    (easy-menu-add-item yaham-mode-menu '("Skeletons") (vector menuitem sym :help help))
    (if abbrev (define-abbrev yaham-abbrev-table abbrev "" sym))))

(defvar yaham-skel-alist nil
  "*List of skeletons which are automatically loaded.
Each member is a list of 4 or 5 elements, corresponding to the arguments
of `yaham-skel-def': (SYM SPEC KEYS MENUITEM ABBREV).  
You can add your own skeletons to this list during Emacs startup.
Alternatively, you can just define them in an `eval-after-load' form
when this module is being loaded; then you don't need this variable.")

(defmacro yaham-skel-add-def (sym skel doc spec keys menuitem &optional abbrev)
  "Define a skeleton and add it to `yaham-skel-alist'."
  `(push (list (defconst ,sym ,skel ,doc) ,spec ,keys ,menuitem ,abbrev)
         yaham-skel-alist))

(put 'yaham-skel-add-def 'lisp-indent-function 1)

(yaham-skel-add-def yaham-skel-simple-lambda
  '("\\ " :mark " -> " :yank)
  "Lambda skeleton suited for simple expressions."
  "*" "\C-c1\\" "Simple lambda")

(yaham-skel-add-def yaham-skel-simple-defun
  '(:eval
    (yaham-current-defun)
    " " :mark "\n"
    "= " :in :yank)
  "Skeleton for a single case of function definition."
  "*" "\C-c1f" "Simple defun")

(yaham-skel-add-def yaham-skel-complex-let
  '(:layout?
    ("let\n"
     :in :mark "\n"
     "in " :in :yank)
    ("let {\n"
     :in :mark "\n"
     "} in " :in :yank))
  "Let-in skeleton suited for multiple bindings.
The variable `yaham-skeletons-layout-flag' controls if the layout
or braced version is inserted."
  "*" "\C-c2l" "Complex let-in")

(yaham-skel-add-def yaham-skel-complex-if
  '("if " :mark " then\n"
    :in :yank  "\n"
    "else " :in :mark)
  "If-then-else skeleton suited for complex expressions."
  "*" "\C-c2i" "Complex if-then-else")

(yaham-skel-add-def yaham-skel-complex-do
  '(:layout?
    ("do\n"
     :in :mark "\n"
     "return " :in :yank)
    ("do {\n"
     :in :mark " ;\n"
     "return " :in :yank "\n"
     "}" :in))
  "Skeleton for a do construction returning a value.
The variable `yaham-skeletons-layout-flag' controls if the layout
or braced version is inserted."
  "*" "\C-c2d" "Complex do")

(yaham-skel-add-def yaham-skel-complex-lambda
  '("\\ " :mark " ->\n"
    :in :yank )
  "Lambda skeleton suited for complex expressions."
  "*" "\C-c2\\" "Complex lambda")

(yaham-skel-add-def yaham-skel-complex-defun
  '(:eval
    (yaham-current-defun)
    " " :mark "\n"
    "= " :in :yank
    :eval (1- (nth 0 yaham-skel-args)) ;get repeat count for following subskeleton
    (:nl
     :eval (yaham-current-defun) " " :in :mark "\n"
     "= " :in :mark))
  "Skeleton for multiple cases of function definition.
When used as a command, the numeric prefix argument is the repeat count."
  "*p" "\C-c2f" "Complex defun")

(yaham-skel-add-def yaham-skel-simple-let
  '("let " :mark "\n"
    "in " :in :yank)
  "Let-in skeleton suited for a single binding."
  "*" "\C-c1l" "Simple let-in" "let")

(yaham-skel-add-def yaham-skel-simple-if
  '("if " :mark " then " :yank "\n"
    "else " :in :mark )
  "If-then-else skeleton suited for simple expressions."
  "*" "\C-c1i" "Simple if-then-else" "if")

(yaham-skel-add-def yaham-skel-simple-do
  '(:layout?
    ("do\n"
     :in :yank)
    ("do {\n"
     :in :yank "\n"
     "}" :in ))
  "Skeleton for a do construction returning no value.
The variable `yaham-skeletons-layout-flag' controls if the layout
or braced version is inserted."
  "*" "\C-c1d" "Simple do" "do")

(yaham-skel-add-def yaham-skel-where
  '(:layout?
    ("where\n"
     :in :yank)
    ("where {\n"
     :in :yank "\n"
     "}" :in))
  "Skeleton for a where construction.
The variable `yaham-skeletons-layout-flag' controls if the layout
or braced version is inserted."
  "*" "\C-c2w" "Where" "where")

(yaham-skel-add-def yaham-skel-case
  '(:layout?
    ("case " :mark " of\n"
     :in :mark " -> " :yank)
    ("case " :mark " of {\n"
     :in :mark " -> " :yank "\n"
     "}" :in))
  "Skeleton for a case-of construction.
The variable `yaham-skeletons-layout-flag' controls if the layout
or braced version is inserted."
  "*" "\C-c2c" "Case-of" "case")

(yaham-skel-add-def yaham-skel-tuple
  '( "(" :yank
     :eval (1- (nth 0 yaham-skel-args)) ;get repeat count for following subskeleton
     (", " :mark)
     ")")
  "Skeleton for a tuple.
When used as a command, the numeric prefix argument is the number of components."
  "*p" "\C-c2(" "Tuple")

(yaham-skel-add-def yaham-skel-list
  '( "[" :yank
     :eval (1- (nth 0 yaham-skel-args)) ;get repeat count for following subskeleton
     (", " :mark)
     "]")
  "Skeleton for a list.
When used as a command, the numeric prefix argument is the number of components."
  "*p" "\C-c2[" "List")

(unless (featurep 'yaham-skel)
  (setq yaham-skel-alist
        (sort yaham-skel-alist
              (lambda (def1 def2)
                (string< (nth 3 def1) (nth 3 def2)))))
  (mapc
   (lambda (def)
     (let ((sym (nth 0 def))
           (spec (nth 1 def))
           (keys (nth 2 def))
           (menuitem (nth 3 def))
           (abbrev (nth 4 def)))
       (yaham-skel-def sym spec keys menuitem abbrev)))
   yaham-skel-alist))

(provide 'yaham-skel)


;; Local Variables:
;; generated-autoload-file: "/home/itz/src/yaham/yaham-autoloads.el"
;; End:

;;; yaham-skel.el ends here
