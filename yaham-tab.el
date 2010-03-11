;;; yaham-tab.el --- indentation submodule of Yaham Haskell mode

;; Copyright (C) 2007 Ian Zimmerman <itz@madbat.mine.nu>

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the conditions spelled out in
;; the file LICENSE are met.

;; $Id: yaham-tab.el 89 2007-09-03 04:36:14Z itz $

(eval-when-compile
  (require 'regexp-opt))

(require 'yaham)

(defvar yaham-tab-list-vector nil
  "A vector of matching delimiter intervals in current buffer.
It's always rebuilt by commands that need it and bound locally
since that is quick enough and avoids cache consistency headaches.")

(defsubst yaham-tab-list-move (start how)
  "Like `down-list' but doesn't throw an error."
  (condition-case nil
      (scan-lists start 1 how)
    (error nil)))

(defun yaham-tab-build-list-vector ()
  (let ((list-alist nil) (stack (list (vector (point-min) (1+ (point-max)) 0))))
    (while stack
      (let* (p np (top (car stack))
               (depth (aref top 2)) (start (aref top 0)) (end (aref top 1)))
        (cond
         ((setq p (yaham-tab-list-move start -1)) ;new nesting level
          (push (vector start p depth) list-alist)
          (if (setq np (yaham-tab-list-move p 1))
              (progn
                (aset top 0 np)
                (push (vector p np (1+ depth)) stack))
            (aset top 0 p)
            (aset top 2 (1+ depth))))
         ((setq p (yaham-tab-list-move start 1)) ;end of containing level
          (push (vector start p depth) list-alist)
          (pop stack))
         (t  
          (push (vector start (1+ (point-max)) depth) list-alist)
          (setq stack nil)))))
    (yaham-list->vector list-alist)))
            
(defsubst yaham-tab-list-interval (p)
  "Return a pair (INDEX . DEPTH) reflecting P's position in the list info vector."
  (let* ((i (yaham-bsearch-pos (1+ p) yaham-tab-list-vector))
         (interval (aref yaham-tab-list-vector i)))
    (cons i (aref interval 2))))

;; let us say a position P1 *dominates* another position P2 when:
;; 1. interval (p1) = i1, interval (p2) = i2, i1 <= i2
;; 2. depth (i1) <= depth (i2)
;; 3. there is no such i that i1 < i < i2, depth (i) < depth (i1),
;;    depth (i) < depth (i2)
(defun yaham-tab-dominates-p (p1 p2)
  (let* ((int1 (yaham-tab-list-interval p1)) (int2 (yaham-tab-list-interval p2))
         (i1 (car int1)) (i2 (car int2)) (d1 (cdr int1)) (d2 (cdr int2)))
    (and (<= i1 i2) (<= d1 d2)
         (catch 'found
           (setq i1 (1+ i1))
           (while (< i1 i2)
             (let* ((int (aref yaham-tab-list-vector i1))
                    (d (aref int 2)))
               (if (< d d1) (throw 'found nil)))
             (setq i1 (1+ i1)))
           t))))

(defun yaham-tab-indent-point ()
  "Assume point at start of new line, find position to which we'll indent.

Return nil if there's no good one or if we hit blank line while
searching."
  ;; are we on the first line of the buffer?
  (if (looking-back "\\`[ \t]*") nil
    (save-excursion
      ;; first skip over closing delimiters, because we should indent
      ;; relative to the line that opened them
      (if (looking-at "\\([ \t]*\\s)\\)+") (goto-char (match-end 0)))
      (let ((p (point)))
        (forward-line -1)
        (catch 'found
          (while (not (bobp))
            (cond
             ;; originally, we stopped the search on any ``blank'' line, i.e.
             ;; one containing nothing but tabs and spaces.  But that makes it
             ;; hard to indent skeletons, so now only a completely empty line
             ;; stops the search, i.e. a lone newline.
             ((looking-at "^$")
              (throw 'found nil))
             ((looking-at "[ \t]+$")
              nil)
             ((looking-at (concat "[ \t]*" comment-start-skip))
              nil)
             ((yaham-in-syntax-p (point) (1+ (point)))
              nil)
             ((yaham-tab-dominates-p (point) p)
              (throw 'found (point)))
             (t nil))
            (forward-line -1))
          nil)))))           

(defsubst yaham-tab-virtual-indent (p)
  (save-excursion
    (goto-char p)
    (or (progn
          (back-to-indentation)
          (get-text-property (point) 'yaham-indent-cache))
        (current-indentation))))

(defun yaham-tab-matching-indent ()
  "If we're at a line starting \"in\" or \"else\", determine the indentation
of the matching \"let\" or \"if\"."
  (when (with-syntax-table yaham-alt-syntax-table
          (looking-at
           (eval-when-compile
             (regexp-opt '("in" "else") 'words))))
    (let ((closer (match-string-no-properties 0)) opener either
          (level 1)
          (p0 (point))
          (limit (or (condition-case nil
                         (save-excursion
                           (up-list -1)
                           (point))
                       (error nil))
                     (save-excursion
                       (yaham-beginning-of-defun-internal)
                       (point)))))
      (if (string= closer "in")
          (setq opener "let"
                either (eval-when-compile (regexp-opt '("in" "let") 'words)))
        (setq opener "if"
              either (eval-when-compile (regexp-opt '("else" "if") 'words))))
      (save-excursion
        (catch 'limit
          (while (< 0 level)
            (unless (with-syntax-table yaham-alt-syntax-table
                      (re-search-backward either limit 'move))
              (throw 'limit nil))
            (cond
             ((yaham-out-of-syntax (match-beginning 0) (match-end 0) 0)
              nil)
             ((not (yaham-tab-dominates-p (point) p0))
              (up-list -1))
             ((looking-at closer)
              (setq level (1+ level)))
             ((looking-at opener)
              (setq level (1- level)))
             (t
              (error "Yaham indentation internal error"))))
          (yaham-tab-virtual-indent (point)))))))

(defun yaham-tab-at-data-p ()
  "Check if we are on the second line of a data declaration."
  (let ((case-fold-search nil) (p (yaham-tab-indent-point)))
    (and p (looking-at "[ \t]*=[^-!#$%&*+./<=>?@\\^|~]")
         (save-excursion
           (goto-char p)
           (with-syntax-table yaham-alt-syntax-table
             (looking-at "[ \t]*data\\>"))))))

(defsubst yaham-tab-prev-token ()
  "Go backward to next token boundary, skipping over comments."
  (while (forward-comment -1))
  (point))

(defun yaham-tab-master-indent ()
  "If current line was indented manually to another, return the amount."
  (let* ((m-prop (yaham-search-master (point)))
         (m (car m-prop)) (prop (cdr m-prop)))
    (cond
     ((and m prop)
      (let ((n (get-text-property m prop))
            (base (yaham-tab-virtual-indent m)))
        (+ base (* n yaham-basic-indent))))
     (prop
      (yaham-unlink-from-master (point) m-prop)
      nil)
     (t nil))))

(defun yaham-tab-indent-amount ()
  "Assume point at start of new line, determine indentation amount."
  (let ((syntax-pt
         (save-excursion
           (and (yaham-out-of-syntax (point) (1+ (point)) 0)
                (point)))))
    (if syntax-pt (yaham-syntax-column syntax-pt)
      (or (yaham-tab-master-indent)
          (yaham-tab-matching-indent)
          (let ((indent-pt (yaham-tab-indent-point)))
            (if (not indent-pt) 0
              (let ((case-fold-search nil)
                    (prev-indent (yaham-tab-virtual-indent indent-pt)))
                (cond
                 ;; bar continuing a datatype declaration, guarded equation or guarded pat
                 ((and (looking-at "|[^-!#$%&*+./<=>?@\\^|~]")
                       (save-excursion
                         (goto-char indent-pt)
                         (or (looking-at "[ \t]*|[^-!#$%&*+./<=>?@\\^|~]")
                             (yaham-tab-at-data-p))))
                  prev-indent)
                 ;; right hand side of a (possibly guarded) equation or type declaration
                 ((looking-at "[=|][^-!#$%&*+./<=>?@\\^|~]")
                  (+ yaham-basic-indent prev-indent))
                 ;; right hand side of an assignment, pattern case, or type constraint
                 ((save-excursion
                    (yaham-tab-prev-token)
                    (looking-back "\\(\\`\\|[^-!#$%&*+./<=>?@\\^|~]\\)\\(<-\\|[-=]>\\|::\\)"))
                  (+ yaham-basic-indent prev-indent))
                 ;; continuing definition by cases
                 ((and (with-syntax-table yaham-alt-syntax-table
                         (looking-at "\\sw+"))
                       (string= (match-string-no-properties 0) (yaham-current-defun)))
                  0)
                 ;; closing delimiters
                 ((looking-at "\\([ \t]*\\s)\\)+")
                  prev-indent)
                 ;; layout triggers and split conditionals
                 ((save-excursion
                    (yaham-tab-prev-token)
                    (with-syntax-table yaham-alt-syntax-table
                      (looking-back
                       (eval-when-compile
                         (concat
                          "\\({\\|"
                          (regexp-opt '("let" "do" "of" "where" "then" "else") 'words)
                          "\\)")))))
                  (+ yaham-basic-indent prev-indent))
                 ;; this is mostly for skeletons
                 ((save-excursion
                    (yaham-tab-prev-token)
                    (looking-back "^[ \t]*;"))
                  (1- prev-indent))
                 ;; opening delimiters
                 (t 
                  (save-excursion
                    (if (looking-at "\\([ \t]*\\s)\\)+") (goto-char (match-end 0)))
                    (let ((dnow (cdr (yaham-tab-list-interval (point))))
                          (dprev (cdr (yaham-tab-list-interval indent-pt))))
                      (if (< dprev dnow) (+ dnow prev-indent (- dprev))
                        ;; remaining cases: preserve indentation (we'll refine this as needed)
                        prev-indent))))))))))))

(defsubst yaham-tab-line-internal ()
  (back-to-indentation)
  (if (eq this-command 'indent-for-tab-command)
      (let ((m-prop (yaham-search-master (point))))
        (yaham-unlink-from-master (point) m-prop)))
  (indent-line-to (yaham-tab-indent-amount)))

;;;###autoload
(defun yaham-tab-line ()
  "Indent current line according to accepted rules of Haskell coding style."
  (let ((yaham-syntax-vector (yaham-build-syntax-vector))
        (yaham-tab-list-vector (yaham-tab-build-list-vector)))
    (if (<= (current-column) (current-indentation))
        (yaham-tab-line-internal)
      (save-excursion
        (yaham-tab-line-internal)))))

(defun yaham-tab-region-internal (start end)
  (let ((endmark (copy-marker end)))
    (unwind-protect
        (save-excursion
          (goto-char start)
          (when (and (bolp) (not (eolp)))
            (back-to-indentation)
            (put-text-property (point) (1+ (point))
                               'yaham-indent-cache (yaham-tab-indent-amount)))
          (forward-line 1)
          (while (and (not (eobp)) (<= (point) endmark))
            (back-to-indentation)
            (put-text-property (point) (1+ (point))
                               'yaham-indent-cache (yaham-tab-indent-amount))
            (forward-line 1))
          (goto-char start)
          (when (and (bolp) (not (eolp)))
            (back-to-indentation)
            (let ((ind (get-text-property (point) 'yaham-indent-cache)))
              (when ind
                (indent-line-to ind)
                (put-text-property (point) (1+ (point))
                                   'yaham-indent-cache nil))))
          (forward-line 1)
          (while (and (not (eobp)) (<= (point) endmark))
            (back-to-indentation)
            (let ((ind (get-text-property (point) 'yaham-indent-cache)))
              (when ind
                (indent-line-to ind)
                (put-text-property (point) (1+ (point))
                                   'yaham-indent-cache nil)))
            (forward-line 1)))
      (set-marker endmark nil))))

;;;###autoload
(defun yaham-tab-region (start end)
  "Indent every line whose first char is within the bounds."
  (let ((yaham-syntax-vector (yaham-build-syntax-vector))
        (yaham-tab-list-vector (yaham-tab-build-list-vector)))
    (yaham-tab-region-internal start end)))

(provide 'yaham-tab)


;; Local Variables:
;; generated-autoload-file: "/home/itz/src/yaham/yaham-autoloads.el"
;; End:

;;; yaham-tab.el ends here
