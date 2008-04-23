;;; sass-mode.el -- Major mode for editing Sass files
;;; Written by Nathan Weizenbaum

;;; Because Sass's indentation schema is similar
;;; to that of YAML and Python, many indentation-related
;;; functions are similar to those in yaml-mode and python-mode.

;;; To install, save this somewhere and add the following to your .emacs file:
;;;
;;; (add-to-list 'load-path "/path/to/sass-mode.el")
;;; (require 'sass-mode nil 't)
;;; (add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
;;;

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

;; Font lock

(defconst sass-font-lock-keywords
  '(("^ *\\(\t\\)"                            1 'haml-tab-face)
    ("^@.*"                                   0 font-lock-constant-face)
    ("\\(\'[^']*'\\)"                         1 font-lock-string-face append)
    ("\\(\"[^\"]*\"\\)"                       1 font-lock-string-face append)
    ("\\(#[0-9a-fA-F]\\{3\\}\\{1,2\\}\\>\\)"  1 font-lock-string-face append)
    ("\\(:[A-Za-z-]+\\|[A-Za-z-]+:\\)"        0 font-lock-constant-face append)
    ("![a-z0-9_-]+"                           0 font-lock-variable-name-face append)
    ("^ *\\(/[/*].*\\)$"                      1 font-lock-comment-face append)
    ("\\(?:^\\|,\\) *\\(#[a-z0-9_-]+\/?\\)"   1 font-lock-keyword-face)
    ("\\(?:^\\|,\\) *\\(\\.[a-z0-9_-]+\/?\\)" 1 font-lock-type-face)
    ("\\(?:^\\|,\\) *\\(&\\|[a-z0-9_]+\/?\\)" 1 font-lock-function-name-face)
    ("\\([=]\\)"                              0 font-lock-preprocessor-face prepend)
    ("\\(?:^\\|,\\) *\\(#[a-z0-9_]+\/?\\)"    (1 font-lock-keyword-face)
     ("\\.[a-z0-9_-]+" nil nil                (0 font-lock-type-face)))
    ("\\(?:^\\|,\\) *\\(\\.[a-z0-9_]+\/?\\)"  (1 font-lock-type-face)
     ("\\.[a-z0-9_-]+" nil nil                (0 font-lock-type-face)))
    ("\\(?:^\\|,\\) *\\(\\.[a-z0-9_]+\/?\\)"  (1 font-lock-type-face)
     ("\\#[a-z0-9_-]+" nil nil                (0 font-lock-keyword-face)))
    ("\\(?:^\\|,\\) *\\(&\\|[a-z0-9_]+\/?\\)" (1 font-lock-function-name-face)
     ("\\.[a-z0-9_-]+" nil nil                (0 font-lock-type-face)))
    ("\\(?:^\\|,\\) *\\(&\\|[a-z0-9_]+\/?\\)" (1 font-lock-function-name-face)
     ("\\#[a-z0-9_-]+" nil nil                (0 font-lock-keyword-face)))))

;; Constants

(defconst sass-blank-line-re "^[ \t]*$"
  "Regexp matching a line containing only whitespace.")

(defconst sass-full-attr-re "^ *:[^ \t]+[ \t]+[^ \t]"
  "Regexp matching a Sass attribute with content.")

;; Mode setup

(define-derived-mode sass-mode haml-mode "Sass"
  "Major mode for editing Sass files."
  (set (make-local-variable 'haml-compute-indentation-function) 'sass-compute-indentation)
  (set (make-local-variable 'haml-indent-offset) sass-indent-offset)
  (setq font-lock-defaults
        '(sass-font-lock-keywords nil t)))

;; Indentation and electric keys

(defun sass-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp) 0
      (forward-line -1)
      (while (and (looking-at sass-blank-line-re)
                  (> (point) (point-min)))
        (forward-line -1))
      (+ (current-indentation)
         (if (not (looking-at sass-full-attr-re))
             sass-indent-offset 0)))))

;; Setup/Activation

(provide 'sass-mode)
