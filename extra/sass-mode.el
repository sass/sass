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

(defface sass-tab-face
   '((((class color)) (:background "red" :foreground "red" :bold t))
     (t (:reverse-video t)))
  "Face to use for highlighting tabs in Sass files."
  :group 'faces
  :group 'sass)

;; Helper Functions

(defun string-* (str n)
  "Concatenates a string with itself n times."
  (if (= n 0) ""
    (concat str (string-* str (- n 1)))))

(defun sre (str)
  "Prepends a Sass-tab-matching regexp to str."
  (concat "^\\(" (string-* " " sass-indent-offset) "\\)*" str))

;; Font lock

(defconst sass-font-lock-keywords
  '(("^@.*"                                   0 font-lock-constant-face)
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

(defconst sass-full-attr-re (sre ":[^ \t]+[ \t]+[^ \t]")
  "Regexp matching a Sass attribute with content.")

;; Mode setup

(defvar sass-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key sass-mode-map [backspace] 'sass-electric-backspace)
    (define-key sass-mode-map "\C-?" 'sass-electric-backspace)
    map))

(define-derived-mode sass-mode fundamental-mode "Sass"
  "Major mode for editing Sass files.

\\{sass-mode-map}"
  (set (make-local-variable 'indent-line-function) 'sass-indent-line)
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

(defun sass-indent-line ()
  "Indent the current line.
The first time this command is used, the line will be indented to the
maximum sensible indentation.  Each immediately subsequent usage will
back-dent the line by `sass-indent-offset' spaces.  On reaching column
0, it will cycle back to the maximum sensible indentation."
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column))
        (need (sass-compute-indentation)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if (and (equal last-command this-command) (/= ci 0))
          (indent-to (* (/ (- ci 1) sass-indent-offset) sass-indent-offset))
        (indent-to need)))
      (if (< (current-column) (current-indentation))
          (forward-to-indentation 0))))

(defun sass-electric-backspace (arg)
  "Delete characters or back-dent the current line.
If invoked following only whitespace on a line, will back-dent to the
immediately previous multiple of `sass-indent-offset' spaces."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column)) (bolp))
      (backward-delete-char arg)
    (let ((ci (current-column)))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to (* (/ (- ci (* arg sass-indent-offset))
                       sass-indent-offset)
                    sass-indent-offset)))))

;; Setup/Activation

(provide 'sass-mode)
