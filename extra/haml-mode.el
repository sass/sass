;;; haml-mode.el -- Major mode for editing Haml files
;;; Written by Nathan Weizenbaum

;;; Because Haml's indentation schema is similar
;;; to that of YAML and Python, many indentation-related
;;; functions are similar to those in yaml-mode and python-mode.

;;; To install, save this somewhere and add the following to your .emacs file:
;;;
;;; (add-to-list 'load-path "/path/to/haml-mode.el")
;;; (require 'haml-mode nil 't)
;;; (add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
;;;

;;; Code:

(eval-when-compile (require 'cl))

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

(defface haml-tab-face
  '((((class color)) (:background "hotpink"))
    (t (:reverse-video t)))
  "Face to use for highlighting tabs in Haml files."
  :group 'faces
  :group 'haml)

(defvar haml-indent-function 'haml-indent-p
  "This function should look at the current line and return true
if the next line could be nested within this line.")

(defvar haml-block-openers
  `("^ *\\([%\\.#][^ \t]*\\)\\(\\[.*\\]\\)?\\({.*}\\)?\\(\\[.*\\]\\)?[ \t]*$"
    "^ *[-=].*do[ \t]*\\(|.*|[ \t]*\\)?$"
    ,(concat "^ *-[ \t]*"
             (regexp-opt '("else" "elsif" "rescue" "ensure" "when")))
    "^ */\\(\\[.*\\]\\)?[ \t]*$"
    "^ *-#"
    "^ *:")
  "A list of regexps that match lines of Haml that could have
text nested beneath them.")

;; Font lock

(defconst haml-font-lock-keywords
  '(("^ *\\(\t\\)"               1 'haml-tab-face)
    ("^!!!.*"                    0 font-lock-constant-face)
    ("\\('[^']*'\\)"             1 font-lock-string-face append)
    ("\\(\"[^\"]*\"\\)"          1 font-lock-string-face append)
    ("&?:\\w+"                   0 font-lock-constant-face append)
    ("@[a-z0-9_]+"               0 font-lock-variable-name-face append)
    ("| *$"                      0 font-lock-string-face)
    ("^[ \t]*\\(/.*\\)$"         1 font-lock-comment-face append)
    ("^ *\\(#[a-z0-9_]+\/?\\)"   1 font-lock-keyword-face)
    ("^ *\\(\\.[a-z0-9_]+\/?\\)" 1 font-lock-type-face)
    ("^ *\\(%[a-z0-9_]+\/?\\)"   1 font-lock-function-name-face)
    ("^ *\\(#[a-z0-9_]+\/?\\)"   (1 font-lock-keyword-face)
     ("\\.[a-z0-9_]+" nil nil    (0 font-lock-type-face)))
    ("^ *\\(\\.[a-z0-9_]+\/?\\)" (1 font-lock-type-face)
     ("\\.[a-z0-9_]+" nil nil    (0 font-lock-type-face)))
    ("^ *\\(\\.[a-z0-9_]+\/?\\)" (1 font-lock-type-face)
     ("\\#[a-z0-9_]+" nil nil    (0 font-lock-keyword-face)))
    ("^ *\\(%[a-z0-9_]+\/?\\)"   (1 font-lock-function-name-face)
     ("\\.[a-z0-9_]+" nil nil    (0 font-lock-type-face)))
    ("^ *\\(%[a-z0-9_]+\/?\\)"   (1 font-lock-function-name-face)
     ("\\#[a-z0-9_]+" nil nil    (0 font-lock-keyword-face)))
    ("^ *\\([~=-] .*\\)"         1 font-lock-preprocessor-face prepend)
    ("^ *[\\.#%a-z0-9_]+\\([~=-] .*\\)"     1 font-lock-preprocessor-face prepend)
    ("^ *[\\.#%a-z0-9_]+\\({[^}]+}\\)"      1 font-lock-preprocessor-face prepend)
    ("^ *[\\.#%a-z0-9_]+\\(\\[[^]]+\\]\\)"  1 font-lock-preprocessor-face prepend)))

;; Mode setup

(defvar haml-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table in use in haml-mode buffers.")

(defvar haml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [backspace] 'haml-electric-backspace)
    (define-key map "\C-?" 'haml-electric-backspace)
    map))

(define-derived-mode haml-mode fundamental-mode "Haml"
  "Major mode for editing Haml files.

\\{haml-mode-map}"
  (set-syntax-table haml-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'haml-indent-line)
  (set (make-local-variable 'indent-region-function) 'haml-indent-region)
  (setq font-lock-defaults '((haml-font-lock-keywords) nil t)))

;; Indentation and electric keys

(defun haml-indent-p ()
  "Returns true if the current line can have lines nested beneath it."
  (loop for opener in haml-block-openers
        if (looking-at opener) return t
        return nil))

(defun haml-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp) 0
      (loop do (forward-line -1)
            while (and (looking-at "^[ \t]$")
                       (> (point) (point-min))))
      (+ (current-indentation)
         (if (funcall haml-indent-function) haml-indent-offset
           0)))))

(defun haml-indent-region (start end)
  "Indent each nonblank line in the region.
This is done by indenting the first line based on
`haml-compute-indentation' and preserving the relative
indentation of the rest of the region.

If this command is used multiple times in a row, it will cycle
between possible indentations."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    ;; Don't start in the middle of a line
    (unless (bolp) (forward-line 1))
    (let (this-line-column current-column
          (next-line-column
           (if (and (equal last-command this-command) (/= (current-indentation) 0))
               (* (/ (- (current-indentation) 1) haml-indent-offset) haml-indent-offset)
             (haml-compute-indentation))))
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
        (cc (current-column))
        (need (haml-compute-indentation)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if (and (equal last-command this-command) (/= ci 0))
          (indent-to (* (/ (- ci 1) haml-indent-offset) haml-indent-offset))
        (indent-to need)))
      (if (< (current-column) (current-indentation))
          (forward-to-indentation 0))))

(defun haml-electric-backspace (arg)
  "Delete characters or back-dent the current line.
If invoked following only whitespace on a line, will back-dent to the
immediately previous multiple of `haml-indent-offset' spaces."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column)) (bolp))
      (backward-delete-char arg)
    (let ((ci (current-column)))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to (* (/ (- ci (* arg haml-indent-offset))
                       haml-indent-offset)
                    haml-indent-offset)))))

;; Setup/Activation

(provide 'haml-mode)
