;;; sass-mode.el --- Major mode for editing Sass files

;; Copyright (c) 2007, 2008 Nathan Weizenbaum

;; Author: Nathan Weizenbaum
;; URL: http://github.com/nex3/haml/tree/master
;; Version: 1.0
;; Keywords: markup, language

;;; Commentary:

;; Because Sass's indentation schema is similar
;; to that of YAML and Python, many indentation-related
;; functions are similar to those in yaml-mode and python-mode.

;; To install, save this on your load path and add the following to
;; your .emacs file:
;;
;; (require 'sass-mode)

;;; Code:

(require 'haml-mode)

;; User definable variables

(defgroup sass nil
  "Support for the Sass template language."
  :group 'languages
  :prefix "sass-")

(defcustom sass-mode-hook nil
  "Hook run when entering Sass mode."
  :type 'hook
  :group 'sass)

(defcustom sass-indent-offset 2
  "Amount of offset per level of indentation."
  :type 'integer
  :group 'sass)

(defvar sass-non-block-openers
  '("^ *:[^ \t]+[ \t]+[^ \t]"
    "^ *[^ \t:]+[ \t]*[=:][ \t]*[^ \t]")
  "A list of regexps that match lines of Sass that couldn't have
text nested beneath them.")

;; Font lock

(defconst sass-selector-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    st))

(defconst sass-selector-font-lock-keywords
  '(("&"       0 font-lock-constant-face)
    ("\\.\\w+" 0 font-lock-type-face)
    ("#\\w+"   0 font-lock-keyword-face)
    ;; Pseudo-selectors, optionally with arguments (e.g. :first, :nth-child(12))
    ("\\(::?\\w+\\)" (1 font-lock-function-name-face)
     ("(\\([^)]+\\))" nil nil (1 font-lock-string-face)))
    ;; Attribute selectors (e.g. p[foo=bar])
    ("\\[\\([^]=]+\\)" (1 font-lock-variable-name-face)
     ("[~|$^*]?=\\([^]=]+\\)" nil nil (1 font-lock-string-face)))))

(defconst sass-font-lock-keywords
  '((sass-highlight-line 1 nil nil t)))

(defconst sass-line-keywords
  '(("@.*"     0 font-lock-constant-face)
    ("/[/*].*" 0 font-lock-comment-face)
    (".*"      sass-highlight-selector))
  "A list of full-line Sass syntax to highlight,
used by `sass-highlight-line'.

Each item is either of the form (REGEXP SUBEXP FACE) or (REGEXP FN).
Each REGEXP is run successively on the beginning of non-whitespace
on the current line until one matches. If it has SUBEXP and FACE,
then SUBEXP is highlighted using FACE. Otherwise, FN is run.")

(defun sass-highlight-line (limit)
  "Highlight a single line using some Sass single-line syntax,
taken from `sass-line-keywords'."
  (save-match-data
    (when (re-search-forward "^ *\\(.+\\)$" limit t)
      (goto-char (match-beginning 1))
      (dolist (keyword sass-line-keywords)
        (destructuring-bind (keyword subexp-or-fn &optional face) keyword
          (when (looking-at keyword)
            (if (integerp subexp-or-fn)
                (put-text-property (match-beginning subexp-or-fn)
                                   (match-end subexp-or-fn)
                                   'face face)
              (funcall subexp-or-fn))
            (end-of-line)
            (return t)))))))

(defun sass-highlight-selector ()
  "Highlight a CSS selector starting at `point'
and ending at `end-of-line'."
  (end-of-line)
  (let ((font-lock-keywords sass-selector-font-lock-keywords)
        (font-lock-syntax-table sass-selector-syntax-table))
    (font-lock-fontify-region
     (point) (progn (end-of-line) (point))))
  t)

;; Constants

;; Mode setup

;;;###autoload
(define-derived-mode sass-mode haml-mode "Sass"
  "Major mode for editing Sass files."
  (set-syntax-table (make-syntax-table))
  (setq font-lock-extend-region-functions
        '(font-lock-extend-region-wholelines font-lock-extend-region-multiline))
  (setq font-lock-multiline nil)
  (setq comment-start "/*")
  (set (make-local-variable 'haml-indent-function) 'sass-indent-p)
  (set (make-local-variable 'haml-indent-offset) sass-indent-offset)
  (setq font-lock-defaults '(sass-font-lock-keywords nil t)))

;; Indentation

(defun sass-indent-p ()
  "Returns t if the current line can have lines nested beneath it."
  (loop for opener in sass-non-block-openers
        unless (looking-at opener) return t
        return nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

;; Setup/Activation
(provide 'sass-mode)
;;; sass-mode.el ends here
