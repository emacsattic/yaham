;;; yaham-autoloads.el --- autoloads for Yaham package

;; Copyright (C) 2007 Ian Zimmerman <itz@madbat.mine.nu>

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the conditions spelled out in
;; the file LICENSE are met.

;; $Id: yaham-autoloads.el 85 2007-08-05 04:24:18Z itz $


;;;### (autoloads (yaham-create-tags-table yaham-toggle-literate
;;;;;;  yaham-mode) "yaham" "yaham.el" (18025 65049))
;;; Generated autoloads from yaham.el

(autoload (quote yaham-mode) "yaham" "\
Major mode for editing Haskell code.
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
" t nil)

(autoload (quote yaham-toggle-literate) "yaham" "\
Toggle view of literate Haskell source between Yaham and LaTeX mode.
Without argument it toggles between Yaham mode and the normal mode,
which will usually be some variant of `latex-mode' depending on
the filename and `auto-mode-alist'.  With a positive argument ARG it
always narrows and enters Yaham mode, with negative or zero ARG it
widens and enters the normal mode, and with ARG non-nil but not an integer
it narrows but prompts for the mode to enter." t nil)

(autoload (quote yaham-create-tags-table) "yaham" "\
Write a TAGS file of all Haskell files in directory D.
If the optional flag VISIT is non-nil, visit the tags table when done.
Currently, this will not tag Bird type literate sources correctly,
and there are no plans to fix that." t nil)

;;;***

;;;### (autoloads (yaham-hugs-mode) "yaham-hugs" "yaham-hugs.el"
;;;;;;  (18025 65140))
;;; Generated autoloads from yaham-hugs.el

(autoload (quote yaham-hugs-mode) "yaham-hugs" "\
A specialization of `yaham-mode' for Hugs." t nil)

;;;***

;;;### (autoloads (yaham-tab-region yaham-tab-line) "yaham-tab" "yaham-tab.el"
;;;;;;  (18025 23425))
;;; Generated autoloads from yaham-tab.el

(autoload (quote yaham-tab-line) "yaham-tab" "\
Indent current line according to accepted rules of Haskell coding style." nil nil)

(autoload (quote yaham-tab-region) "yaham-tab" "\
Indent every line whose first char is within the bounds." nil nil)

;;;***


(provide 'yaham-autoloads)

;;; yaham-autoloads.el ends here
