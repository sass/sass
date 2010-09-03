;;; haml-mode.el --- Major mode for editing Haml files

;; Copyright (c) 2007, 2008 Nathan Weizenbaum

;; Author: Nathan Weizenbaum
;; URL: http://github.com/nex3/haml/tree/master
;; Version: 3.0.14
;; Created: 2007-03-08
;; By: Nathan Weizenbaum
;; Keywords: markup, language, html

;;; Commentary:

;; Because Haml's indentation schema is similar
;; to that of YAML and Python, many indentation-related
;; functions are similar to those in yaml-mode and python-mode.

;; To install, save this on your load path and add the following to
;; your .emacs file:
;;
;; (require 'haml-mode)

;;; Code:

(eval-when-compile (require 'cl))
(require 'ruby-mode)

;; Additional (optional) libraries for fontification
(require 'css-mode nil t)
(require 'textile-mode nil t)
(require 'markdown-mode nil t)
(require 'javascript-mode "javascript" t)
(require 'js nil t)


;; User definable variables

(defgroup haml nil
  "Support for the Haml template language."
  :group 'languages
  :prefix "haml-")

(defcustom haml-mode-hook nil
  "Hook run when entering Haml mode."
  :type 'hook
  :group 'haml)

(defcustom haml-indent-offset 2
  "Amount of offset per level of indentation."
  :type 'integer
  :group 'haml)

(defcustom haml-backspace-backdents-nesting t
  "Non-nil to have `haml-electric-backspace' re-indent blocks of code.
This means that all code nested beneath the backspaced line is
re-indented along with the line itself."
  :type 'boolean
  :group 'haml)

(defvar haml-indent-function 'haml-indent-p
  "A function for checking if nesting is allowed.
This function should look at the current line and return t
if the next line could be nested within this line.

The function can also return a positive integer to indicate
a specific level to which the current line could be indented.")

(defconst haml-tag-beg-re
  "^[ \t]*\\(?:[%\\.#][a-z0-9_:\\-]*\\)+\\(?:(.*)\\|{.*}\\|\\[.*\\]\\)*"
  "A regexp matching the beginning of a Haml tag, through (), {}, and [].")

(defvar haml-block-openers
  `(,(concat haml-tag-beg-re "[><]*[ \t]*$")
    "^[ \t]*[&!]?[-=~].*do[ \t]*\\(|.*|[ \t]*\\)?$"
    ,(concat "^[ \t]*[&!]?[-=~][ \t]*\\("
             (regexp-opt '("if" "unless" "while" "until" "else"
                           "begin" "elsif" "rescue" "ensure" "when"))
             "\\)")
    "^[ \t]*/\\(\\[.*\\]\\)?[ \t]*$"
    "^[ \t]*-#"
    "^[ \t]*:")
  "A list of regexps that match lines of Haml that open blocks.
That is, a Haml line that can have text nested beneath it should
be matched by a regexp in this list.")

;; Font lock

(defun haml-nested-regexp (re)
  "Create a regexp to match a block starting with RE.
The line containing RE is matched, as well as all lines indented beneath it."
  (concat "^\\([ \t]*\\)" re "\\(\n\\(?:\\(?:\\1 .*\\| *\\)\n\\)*\\(?:\\1 .*\\| *\\)?\\)?"))

(defconst haml-font-lock-keywords
  `((,(haml-nested-regexp "\\(?:-#\\|/\\).*")  0 font-lock-comment-face)
    (,(haml-nested-regexp ":\\w+") 0 font-lock-string-face)
    (haml-highlight-ruby-filter-block     1 font-lock-preprocessor-face)
    (haml-highlight-css-filter-block      1 font-lock-preprocessor-face)
    (haml-highlight-textile-filter-block  1 font-lock-preprocessor-face)
    (haml-highlight-markdown-filter-block 1 font-lock-preprocessor-face)
    (haml-highlight-js-filter-block       1 font-lock-preprocessor-face)
    (haml-highlight-interpolation         1 font-lock-variable-name-face prepend)
    (haml-highlight-ruby-tag              1 font-lock-preprocessor-face)
    (haml-highlight-ruby-script           1 font-lock-preprocessor-face)
    ("^!!!.*"                             0 font-lock-constant-face)
    ("| *$"                               0 font-lock-string-face)))

(defconst haml-filter-re "^[ \t]*:\\w+")
(defconst haml-comment-re "^[ \t]*\\(?:-\\#\\|/\\)")

(defun haml-fontify-region (beg end keywords syntax-table syntactic-keywords)
  "Fontify a region between BEG and END using another mode's fontification.

KEYWORDS, SYNTAX-TABLE, and SYNTACTIC-KEYWORDS are the values of that mode's
`font-lock-keywords', `font-lock-syntax-table',
and `font-lock-syntactic-keywords', respectively."
  (save-excursion
    (save-match-data
      (let ((font-lock-keywords keywords)
            (font-lock-syntax-table syntax-table)
            (font-lock-syntactic-keywords syntactic-keywords)
            (font-lock-multiline 'undecided)
            font-lock-keywords-only
            font-lock-extend-region-functions
            font-lock-keywords-case-fold-search)
        ;; font-lock-fontify-region apparently isn't inclusive,
        ;; so we have to move the beginning back one char
        (font-lock-fontify-region (- beg 1) end)))))

(defun haml-fontify-region-as-ruby (beg end)
  "Use Ruby's font-lock variables to fontify the region between BEG and END."
  (haml-fontify-region beg end ruby-font-lock-keywords
                       ruby-font-lock-syntax-table
                       ruby-font-lock-syntactic-keywords))

(defun haml-handle-filter (filter-name limit fn)
  "If a FILTER-NAME filter is found within LIMIT, run FN on that filter.

FN is passed a pair of points representing the beginning and end
of the filtered text."
  (when (re-search-forward (haml-nested-regexp (concat ":" filter-name)) limit t)
    (funcall fn (+ 2 (match-beginning 2)) (match-end 2))))

(defun haml-fontify-filter-region (filter-name limit &rest fontify-region-args)
  "If a FILTER-NAME filter is found within LIMIT, fontify it.

The fontification is done by passing FONTIFY-REGION-ARGS to
`haml-fontify-region'."
  (haml-handle-filter filter-name limit
                      (lambda (beg end)
                        (apply 'haml-fontify-region
                               (append (list beg end)
                                       fontify-region-args)))))

(defun haml-highlight-ruby-filter-block (limit)
  "If a :ruby filter is found within LIMIT, highlight it."
  (haml-handle-filter "ruby" limit 'haml-fontify-region-as-ruby))

(defun haml-highlight-css-filter-block (limit)
  "If a :css filter is found within LIMIT, highlight it.

This requires that `css-mode' is available.
`css-mode' is included with Emacs 23."
  (if (boundp 'css-font-lock-keywords)
      (haml-fontify-filter-region "css" limit css-font-lock-keywords nil nil)))

(defun haml-highlight-js-filter-block (limit)
  "If a :javascript filter is found within LIMIT, highlight it.

This requires that Karl Landström's javascript mode be available, either as the
\"js.el\" bundled with Emacs 23, or as \"javascript.el\" found in ELPA and
elsewhere."
  (let ((keywords (or (and (featurep 'js) js--font-lock-keywords-3)
                      (and (featurep 'javascript-mode) js-font-lock-keywords-3)))
        (syntax-table (or (and (featurep 'js) js-mode-syntax-table)
                          (and (featurep 'javascript-mode) javascript-mode-syntax-table))))
    (when keywords
      (haml-fontify-filter-region "javascript" limit keywords syntax-table nil))))

(defun haml-highlight-textile-filter-block (limit)
  "If a :textile filter is found within LIMIT, highlight it.

This requires that `textile-mode' be available.

Note that the results are not perfect, since `textile-mode' expects
certain constructs such as \"h1.\" to be at the beginning of a line,
and indented Haml filters always have leading whitespace."
  (if (boundp 'textile-font-lock-keywords)
      (haml-fontify-filter-region "textile" limit textile-font-lock-keywords nil nil)))

(defun haml-highlight-markdown-filter-block (limit)
  "If a :markdown filter is found within LIMIT, highlight it.

This requires that `markdown-mode' be available."
  (if (boundp 'markdown-mode-font-lock-keywords)
      (haml-fontify-filter-region "markdown" limit
                                  markdown-mode-font-lock-keywords
                                  markdown-mode-syntax-table
                                  nil)))

(defun haml-highlight-ruby-script (limit)
  "Highlight a Ruby script expression (-, =, or ~).
LIMIT works as it does in `re-search-forward'."
  (when (re-search-forward "^[ \t]*\\(-\\|[&!]?[=~]\\) \\(.*\\)$" limit t)
    (haml-fontify-region-as-ruby (match-beginning 2) (match-end 2))))

(defun haml-highlight-ruby-tag (limit)
  "Highlight Ruby code within a Haml tag.
LIMIT works as it does in `re-search-forward'.

This highlights the tag attributes and object refs of the tag,
as well as the script expression (-, =, or ~) following the tag.

For example, this will highlight all of the following:
  %p{:foo => 'bar'}
  %p[@bar]
  %p= 'baz'
  %p{:foo => 'bar'}[@bar]= 'baz'"
  (when (re-search-forward "^[ \t]*[%.#]" limit t)
    (forward-char -1)

    ;; Highlight tag, classes, and ids
    (while (haml-move "\\([.#%]\\)[a-z0-9_:\\-]*")
      (put-text-property (match-beginning 0) (match-end 0) 'face
                         (case (char-after (match-beginning 1))
                           (?% font-lock-function-name-face)
                           (?# font-lock-keyword-face)
                           (?. font-lock-type-face))))

    (block loop
      (while t
        (let ((eol (save-excursion (end-of-line) (point))))
          (case (char-after)
            ;; Highlight obj refs
            (?\[
             (let ((beg (point)))
               (haml-limited-forward-sexp eol)
               (haml-fontify-region-as-ruby beg (point))))
            ;; Highlight new attr hashes
            (?\(
             (forward-char 1)
             (while
                 (and (haml-parse-new-attr-hash
                       (lambda (type beg end)
                         (case type
                           (name (put-text-property beg end 'face font-lock-constant-face))
                           (value (haml-fontify-region-as-ruby beg end)))))
                      (not (eobp)))
               (forward-line 1)
               (beginning-of-line)))
            ;; Highlight old attr hashes
            (?\{
             (let ((beg (point)))
               (haml-limited-forward-sexp eol)

               ;; Check for multiline
               (while (and (eolp) (eq (char-before) ?,) (not (eobp)))
                 (forward-line)
                 (let ((eol (save-excursion (end-of-line) (point))))
                   ;; If no sexps are closed,
                   ;; we're still continuing a  multiline hash
                   (if (>= (car (parse-partial-sexp (point) eol)) 0)
                       (end-of-line)
                     ;; If sexps have been closed,
                     ;; set the point at the end of the total sexp
                     (goto-char beg)
                     (haml-limited-forward-sexp eol))))

               (haml-fontify-region-as-ruby (+ 1 beg) (point))))
            (t (return-from loop))))))

    ;; Move past end chars
    (when (looking-at "[<>&!]+") (goto-char (match-end 0)))
    ;; Highlight script
    (if (looking-at "\\([=~]\\) \\(.*\\)$")
        (haml-fontify-region-as-ruby (match-beginning 2) (match-end 2))
      ;; Give font-lock something to highlight
      (forward-char -1)
      (looking-at "\\(\\)"))
    t))

(defun haml-move (re)
  "Try matching and moving to the end of regular expression RE.
Returns non-nil if the expression was sucessfully matched."
  (when (looking-at re)
    (goto-char (match-end 0))
    t))

(defun haml-highlight-interpolation (limit)
  "Highlight Ruby interpolation (#{foo}).
LIMIT works as it does in `re-search-forward'."
  (when (re-search-forward "\\(#{\\)" limit t)
    (save-match-data
      (forward-char -1)
      (let ((beg (point)))
        (haml-limited-forward-sexp limit)
        (haml-fontify-region-as-ruby (+ 1 beg) (point)))

      (when (eq (char-before) ?})
        (put-text-property (- (point) 1) (point)
                           'face font-lock-variable-name-face))
      t)))

(defun haml-limited-forward-sexp (limit &optional arg)
  "Move forward using `forward-sexp' or to LIMIT, whichever comes first.
With ARG, do it that many times."
  (let (forward-sexp-function)
    (condition-case err
        (save-restriction
          (narrow-to-region (point) limit)
          (forward-sexp arg))
      (scan-error
       (unless (equal (nth 1 err) "Unbalanced parentheses")
         (signal 'scan-error (cdr err)))
       (goto-char limit)))))

(defun* haml-extend-region-filters-comments ()
  "Extend the font-lock region to encompass filters and comments."
  (let ((old-beg font-lock-beg)
        (old-end font-lock-end))
    (save-excursion
      (goto-char font-lock-beg)
      (beginning-of-line)
      (unless (or (looking-at haml-filter-re)
                  (looking-at haml-comment-re))
        (return-from haml-extend-region-filters-comments))
      (setq font-lock-beg (point))
      (haml-forward-sexp)
      (beginning-of-line)
      (setq font-lock-end (max font-lock-end (point))))
    (or (/= old-beg font-lock-beg)
        (/= old-end font-lock-end))))

(defun* haml-extend-region-multiline-hashes ()
  "Extend the font-lock region to encompass multiline attribute hashes."
  (let ((old-beg font-lock-beg)
        (old-end font-lock-end))
    (save-excursion
      (goto-char font-lock-beg)
      (let ((attr-props (haml-parse-multiline-attr-hash))
            multiline-end)
        (when attr-props
          (setq font-lock-beg (cdr (assq 'point attr-props)))

          (end-of-line)
          ;; Move through multiline attrs
          (when (eq (char-before) ?,)
            (save-excursion
              (while (progn (end-of-line)
                            (and (eq (char-before) ?,) (not (eobp))))
                (forward-line))

              (forward-line -1)
              (end-of-line)
              (setq multiline-end (point))))

          (goto-char (+ (cdr (assq 'point attr-props))
                        (cdr (assq 'hash-indent attr-props))
                        -1))
          (haml-limited-forward-sexp
           (or multiline-end
               (save-excursion (end-of-line) (point))))
          (setq font-lock-end (max font-lock-end (point))))))
    (or (/= old-beg font-lock-beg)
        (/= old-end font-lock-end))))


;; Mode setup

(defvar haml-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table in use in `haml-mode' buffers.")

(defvar haml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [backspace] 'haml-electric-backspace)
    (define-key map "\C-?" 'haml-electric-backspace)
    (define-key map "\C-c\C-f" 'haml-forward-sexp)
    (define-key map "\C-c\C-b" 'haml-backward-sexp)
    (define-key map "\C-c\C-u" 'haml-up-list)
    (define-key map "\C-c\C-d" 'haml-down-list)
    (define-key map "\C-c\C-k" 'haml-kill-line-and-indent)
    (define-key map "\C-c\C-r" 'haml-output-region)
    (define-key map "\C-c\C-l" 'haml-output-buffer)
    map))

;;;###autoload
(define-derived-mode haml-mode fundamental-mode "Haml"
  "Major mode for editing Haml files.

\\{haml-mode-map}"
  (set-syntax-table haml-mode-syntax-table)
  (add-to-list 'font-lock-extend-region-functions 'haml-extend-region-filters-comments)
  (add-to-list 'font-lock-extend-region-functions 'haml-extend-region-multiline-hashes)
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'indent-line-function) 'haml-indent-line)
  (set (make-local-variable 'indent-region-function) 'haml-indent-region)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (setq comment-start "-#")
  (setq font-lock-defaults '((haml-font-lock-keywords) t t)))

;; Useful functions

(defun haml-comment-block ()
  "Comment the current block of Haml code."
  (interactive)
  (save-excursion
    (let ((indent (current-indentation)))
      (back-to-indentation)
      (insert "-#")
      (newline)
      (indent-to indent)
      (beginning-of-line)
      (haml-mark-sexp)
      (haml-reindent-region-by haml-indent-offset))))

(defun haml-uncomment-block ()
  "Uncomment the current block of Haml code."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (not (looking-at haml-comment-re))
      (haml-up-list)
      (beginning-of-line))
    (haml-mark-sexp)
    (kill-line 1)
    (haml-reindent-region-by (- haml-indent-offset))))

(defun haml-replace-region (start end)
  "Replace the current block of Haml code with the HTML equivalent.
Called from a program, START and END specify the region to indent."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (let ((ci (current-indentation)))
      (while (re-search-forward "^ +" end t)
        (replace-match (make-string (- (current-indentation) ci) ? ))))
    (shell-command-on-region start end "haml" "haml-output" t)))

(defun haml-output-region (start end)
  "Displays the HTML output for the current block of Haml code.
Called from a program, START and END specify the region to indent."
  (interactive "r")
  (kill-new (buffer-substring start end))
  (with-temp-buffer
    (yank)
    (haml-indent-region (point-min) (point-max))
    (shell-command-on-region (point-min) (point-max) "haml" "haml-output")))

(defun haml-output-buffer ()
  "Displays the HTML output for entire buffer."
  (interactive)
  (haml-output-region (point-min) (point-max)))

;; Navigation

(defun haml-forward-through-whitespace (&optional backward)
  "Move the point forward through any whitespace.
The point will move forward at least one line, until it reaches
either the end of the buffer or a line with no whitespace.

If BACKWARD is non-nil, move the point backward instead."
  (let ((arg (if backward -1 1))
        (endp (if backward 'bobp 'eobp)))
    (loop do (forward-line arg)
          while (and (not (funcall endp))
                     (looking-at "^[ \t]*$")))))

(defun haml-at-indent-p ()
  "Return non-nil if the point is before any text on the line."
  (let ((opoint (point)))
    (save-excursion
      (back-to-indentation)
      (>= (point) opoint))))

(defun haml-forward-sexp (&optional arg)
  "Move forward across one nested expression.
With ARG, do it that many times.  Negative arg -N means move
backward across N balanced expressions.

A sexp in Haml is defined as a line of Haml code as well as any
lines nested beneath it."
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (< arg 0) (not (haml-at-indent-p)))
      (back-to-indentation)
    (while (/= arg 0)
      (let ((indent (current-indentation)))
        (loop do (haml-forward-through-whitespace (< arg 0))
              while (and (not (eobp))
                         (not (bobp))
                         (> (current-indentation) indent)))
        (back-to-indentation)
        (setq arg (+ arg (if (> arg 0) -1 1)))))))

(defun haml-backward-sexp (&optional arg)
  "Move backward across one nested expression.
With ARG, do it that many times.  Negative arg -N means move
forward across N balanced expressions.

A sexp in Haml is defined as a line of Haml code as well as any
lines nested beneath it."
  (interactive "p")
  (haml-forward-sexp (if arg (- arg) -1)))

(defun haml-up-list (&optional arg)
  "Move out of one level of nesting.
With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (loop do (haml-forward-through-whitespace t)
            while (and (not (bobp))
                       (>= (current-indentation) indent)))
      (setq arg (- arg 1))))
  (back-to-indentation))

(defun haml-down-list (&optional arg)
  "Move down one level of nesting.
With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (haml-forward-through-whitespace)
      (when (<= (current-indentation) indent)
        (haml-forward-through-whitespace t)
        (back-to-indentation)
        (error "Nothing is nested beneath this line"))
      (setq arg (- arg 1))))
  (back-to-indentation))

(defun haml-mark-sexp ()
  "Mark the next Haml block."
  (let ((forward-sexp-function 'haml-forward-sexp))
    (mark-sexp)))

(defun haml-mark-sexp-but-not-next-line ()
  "Mark the next Haml block, but not the next line.
Put the mark at the end of the last line of the sexp rather than
the first non-whitespace character of the next line."
  (haml-mark-sexp)
  (set-mark
   (save-excursion
     (goto-char (mark))
     (forward-line -1)
     (end-of-line)
     (point))))

;; Indentation and electric keys

(defun* haml-indent-p ()
  "Returns t if the current line can have lines nested beneath it."
  (let ((attr-props (haml-parse-multiline-attr-hash)))
    (when attr-props
      (return-from haml-indent-p
        (if (haml-unclosed-attr-hash-p) (cdr (assq 'hash-indent attr-props))
          (list (+ (cdr (assq 'indent attr-props)) haml-indent-offset) nil)))))
  (loop for opener in haml-block-openers
        if (looking-at opener) return t
        finally return nil))

(defun* haml-parse-multiline-attr-hash ()
  "Parses a multiline attribute hash, and returns
an alist with the following keys:

INDENT is the indentation of the line beginning the hash.

HASH-INDENT is the indentation of the first character
within the attribute hash.

POINT is the character position at the beginning of the line
beginning the hash."
  (save-excursion
    (while t
      (beginning-of-line)
      (if (looking-at (concat haml-tag-beg-re "\\([{(]\\)"))
          (progn
            (goto-char (- (match-end 0) 1))
            (haml-limited-forward-sexp (save-excursion (end-of-line) (point)))
            (return-from haml-parse-multiline-attr-hash
              (when (or (string-equal (match-string 1) "(") (eq (char-before) ?,))
                `((indent . ,(current-indentation))
                  (hash-indent . ,(- (match-end 0) (match-beginning 0)))
                  (point . ,(match-beginning 0))))))
        (when (bobp) (return-from haml-parse-multiline-attr-hash))
        (forward-line -1)
        (unless (haml-unclosed-attr-hash-p)
          (return-from haml-parse-multiline-attr-hash))))))

(defun* haml-unclosed-attr-hash-p ()
  "Return t if this line has an unclosed attribute hash, new or old."
  (save-excursion
    (end-of-line)
    (when (eq (char-before) ?,) (return-from haml-unclosed-attr-hash-p t))
    (re-search-backward "(\\|^")
    (haml-move "(")
    (haml-parse-new-attr-hash)))

(defun* haml-parse-new-attr-hash (&optional (fn (lambda (type beg end) ())))
  "Parse a new-style attribute hash on this line, and returns
t if it's not finished on the current line.

FN should take three parameters: TYPE, BEG, and END.
TYPE is the type of text parsed ('name or 'value)
and BEG and END delimit that text in the buffer."
  (let ((eol (save-excursion (end-of-line) (point))))
    (while (not (haml-move ")"))
      (haml-move "[ \t]*")
      (unless (haml-move "[a-z0-9_:\\-]+")
        (return-from haml-parse-new-attr-hash (haml-move "[ \t]*$")))
      (funcall fn 'name (match-beginning 0) (match-end 0))
      (haml-move "[ \t]*")
      (when (haml-move "=")
        (haml-move "[ \t]*")
        (unless (looking-at "[\"'@a-z]") (return-from haml-parse-new-attr-hash))
        (let ((beg (point)))
          (haml-limited-forward-sexp eol)
          (funcall fn 'value beg (point)))
        (haml-move "[ \t]*")))
    nil))

(defun haml-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp) (list 0 nil)
      (haml-forward-through-whitespace t)
      (let ((indent (funcall haml-indent-function)))
        (cond
         ((consp indent) indent)
         ((integerp indent) (list indent t))
         (indent (list (+ (current-indentation) haml-indent-offset) nil))
         (t (list (current-indentation) nil)))))))

(defun haml-indent-region (start end)
  "Indent each nonblank line in the region.
This is done by indenting the first line based on
`haml-compute-indentation' and preserving the relative
indentation of the rest of the region.  START and END specify the
region to indent.

If this command is used multiple times in a row, it will cycle
between possible indentations."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (let (this-line-column current-column
          (next-line-column
           (if (and (equal last-command this-command) (/= (current-indentation) 0))
               (* (/ (- (current-indentation) 1) haml-indent-offset) haml-indent-offset)
             (car (haml-compute-indentation)))))
      (while (< (point) end)
        (setq this-line-column next-line-column
              current-column (current-indentation))
        ;; Delete whitespace chars at beginning of line
        (delete-horizontal-space)
        (unless (eolp)
          (setq next-line-column (save-excursion
                                   (loop do (forward-line 1)
                                         while (and (not (eobp)) (looking-at "^[ \t]*$")))
                                   (+ this-line-column
                                      (- (current-indentation) current-column))))
          ;; Don't indent an empty line
          (unless (eolp) (indent-to this-line-column)))
        (forward-line 1)))
    (move-marker end nil)))

(defun haml-indent-line ()
  "Indent the current line.
The first time this command is used, the line will be indented to the
maximum sensible indentation.  Each immediately subsequent usage will
back-dent the line by `haml-indent-offset' spaces.  On reaching column
0, it will cycle back to the maximum sensible indentation."
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column)))
    (destructuring-bind (need strict) (haml-compute-indentation)
      (save-excursion
        (beginning-of-line)
        (delete-horizontal-space)
        (if (and (not strict) (equal last-command this-command) (/= ci 0))
            (indent-to (* (/ (- ci 1) haml-indent-offset) haml-indent-offset))
          (indent-to need))))
    (when (< (current-column) (current-indentation))
      (forward-to-indentation 0))))

(defun haml-reindent-region-by (n)
  "Add N spaces to the beginning of each line in the region.
If N is negative, will remove the spaces instead.  Assumes all
lines in the region have indentation >= that of the first line."
  (let* ((ci (current-indentation))
         (indent-rx
          (concat "^"
                  (if indent-tabs-mode
                      (concat (make-string (/ ci tab-width) ?\t)
                              (make-string (mod ci tab-width) ?\t))
                    (make-string ci ?\s)))))
    (save-excursion
      (while (re-search-forward indent-rx (mark) t)
        (let ((ci (current-indentation)))
          (delete-horizontal-space)
          (beginning-of-line)
          (indent-to (max 0 (+ ci n))))))))

(defun haml-electric-backspace (arg)
  "Delete characters or back-dent the current line.
If invoked following only whitespace on a line, will back-dent
the line and all nested lines to the immediately previous
multiple of `haml-indent-offset' spaces.  With ARG, do it that
many times.

Set `haml-backspace-backdents-nesting' to nil to just back-dent
the current line."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
          (bolp)
          (looking-at "^[ \t]+$"))
      (backward-delete-char arg)
    (save-excursion
      (let ((ci (current-column)))
        (beginning-of-line)
        (if haml-backspace-backdents-nesting
            (haml-mark-sexp-but-not-next-line)
          (set-mark (save-excursion (end-of-line) (point))))
        (haml-reindent-region-by (* (- arg) haml-indent-offset))
        (pop-mark)))
    (back-to-indentation)))

(defun haml-kill-line-and-indent ()
  "Kill the current line, and re-indent all lines nested beneath it."
  (interactive)
  (beginning-of-line)
  (haml-mark-sexp-but-not-next-line)
  (kill-line 1)
  (haml-reindent-region-by (* -1 haml-indent-offset)))

(defun haml-indent-string ()
  "Return the indentation string for `haml-indent-offset'."
  (mapconcat 'identity (make-list haml-indent-offset " ") ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;; Setup/Activation
(provide 'haml-mode)
;;; haml-mode.el ends here
