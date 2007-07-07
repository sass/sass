;;; haml-mode.el -- Major mode for editing Haml files
;;; Version 0.0.1
;;; Written by Nathan Weizenbaum

;;; Because Haml's indentation schema is similar
;;; to that of YAML and Python, many indentation-related
;;; functions are similar to those in yaml-mode and python-mode.

;;; To install, save this somewhere and add the following to your .emacs file:
;;; 
;;; (add-to-list 'load-path "/path/to/haml-mode.el")
;;; (require 'haml-mode nil 't)
;;; 

;;; Code:

;; User definable variables

(defgroup haml nil
  "Support for the Haml template language."
  :group 'languages
  :prefix "haml-")

(defcustom haml-mode-hook nil
  "*Hook run by `haml-mode'."
  :type 'hook
  :group 'haml)

(defcustom haml-indent-offset 2
  "*Amount of offset per level of indentation."
  :type 'integer
  :group 'haml)

(defcustom haml-backspace-function 'backward-delete-char-untabify
  "*Function called by `haml-electric-backspace' when deleting backwards."
  :type 'function
  :group 'haml)

(defface haml-tab-face
   '((((class color)) (:background "red" :foreground "red" :bold t))
     (t (:reverse-video t)))
  "Face to use for highlighting tabs in Haml files."
  :group 'faces
  :group 'haml)

;; Helper Functions

(defun string-* (str n)
  "Concatenates a string with itself n times."
  (if (= n 0) ""
    (concat str (string-* str (- n 1)))))

(defun find-if (f lst)
  "Returns the first element of a list for which a function returns a non-nil value, or nil if no such element is found."
  (while (not (or (null lst)
                  (apply f (list (car lst)))))
    (setq lst (cdr lst)))
  (if (null lst) nil (car lst)))

(defun hre (str)
  "Prepends a Haml-tab-matching regexp to str."
  (concat "^\\(" (string-* " " haml-indent-offset) "\\)*" str))

;; Font lock

(defconst haml-font-lock-keywords-1
  (list
   ;; instruct
   '("^!!!.*"                    0 font-lock-constant-face)
   ;; strings
   '("\\('[^']*'\\)"             1 font-lock-string-face append)
   '("\\(\"[^\"]*\"\\)"          1 font-lock-string-face append)
   ;; symbol
   '("&?:\\w+"                   0 font-lock-constant-face append)
   ;; ruby varible
   '("@[a-z0-9_]+"               0 font-lock-variable-name-face append)
   ;; pipe
   '("| *$"                      0 font-lock-string-face)
   ;; comment
   '("^[ \t]*\\(/.*\\)$"         1 font-lock-comment-face append)
   ;; id
   '("^ *\\(#[a-z0-9_]+\/?\\)"   1 font-lock-keyword-face)
   ;; class
   '("^ *\\(\\.[a-z0-9_]+\/?\\)" 1 font-lock-type-face)
   ;; tag
   '("^ *\\(%[a-z0-9_]+\/?\\)"   1 font-lock-function-name-face )
   ;; class after id
   '("^ *\\(#[a-z0-9_]+\/?\\)"   (1 font-lock-keyword-face) ("\\.[a-z0-9_]+" nil nil (0 font-lock-type-face)))
   ;; class after class
   '("^ *\\(\\.[a-z0-9_]+\/?\\)" (1 font-lock-type-face) ("\\.[a-z0-9_]+" nil nil (0 font-lock-type-face)))
   ;; id after class
   '("^ *\\(\\.[a-z0-9_]+\/?\\)" (1 font-lock-type-face) ("\\#[a-z0-9_]+" nil nil (0 font-lock-keyword-face)))
   ;; class after tag
   '("^ *\\(%[a-z0-9_]+\/?\\)"   (1 font-lock-function-name-face) ("\\.[a-z0-9_]+" nil nil (0 font-lock-type-face)))
   ;; id after tag
   '("^ *\\(%[a-z0-9_]+\/?\\)"   (1 font-lock-function-name-face) ("\\#[a-z0-9_]+" nil nil (0 font-lock-keyword-face)))
   ;; embeded ruby: beggin of line
   '("^ *\\([~=-] .*\\)"          1 font-lock-preprocessor-face prepend)
   ;; embeded ruby: after tag,class,id
   '("^ *[\\.#%a-z0-9_]+\\([~=-] .*\\)"     1 font-lock-preprocessor-face prepend)
   ;; embeded ruby: attributes
   '("^ *[\\.#%a-z0-9_]+\\({[^}]+}\\)"      1 font-lock-preprocessor-face prepend)
   ;; embeded ruby: square
   '("^ *[\\.#%a-z0-9_]+\\(\\[[^]]+\\]\\)"  1 font-lock-preprocessor-face prepend)))

;; Constants

(defconst haml-mode-version "0.0.1" "Version of `haml-mode.'")

(defconst haml-blank-line-re "^[ \t]*$"
  "Regexp matching a line containing only whitespace.")

; Base for Regexen matching a Haml tag.
(setq haml-tag-re-base (hre "\\([%\\.#][^ \t]*\\)\\({.*}\\)?\\(\\[.*\\]\\)?"))

(defconst haml-tag-nest-re (concat haml-tag-re-base "[ \t]*$")
  "Regexp matching a Haml tag that can have nested elements.")

(defconst haml-tag-re (concat haml-tag-re-base "\\(.?\\)")
  "Regexp matching a Haml tag.")

(defconst haml-block-re (hre "[-=].*do[ \t]*\\(|.*|[ \t]*\\)?$")
  "Regexp matching a Ruby block in Haml.")

(defconst haml-block-cont-re (hre (concat "-[ \t]*"
                                          (regexp-opt '("else" "elsif"
                                                        "rescue" "ensure"
                                                        "when"))))
  "Regexp matching a continued Ruby block in Haml.")

(defconst haml-html-comment-re (hre "/\\(\\[.*\\]\\)?[ \t]*$")
  "Regexp matching a Haml HTML comment command.")

(defconst haml-comment-re (hre "-#[ \t]$")
  "Regexp matching a Haml comment command.")

(defconst haml-filter-re (hre ":")
  "Regexp matching a Haml filter command.")

;; Mode setup

(defvar haml-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table in use in haml-mode buffers.")

(defvar haml-mode-map ()
  "Keymap used in `haml-mode' buffers.")
(if haml-mode-map
    nil
  (setq haml-mode-map (make-sparse-keymap))
  (define-key haml-mode-map [backspace] 'haml-electric-backspace)
  (define-key haml-mode-map "\C-?" 'haml-electric-backspace)
  (define-key haml-mode-map "\C-j" 'newline-and-indent))

(define-derived-mode haml-mode fundamental-mode "Haml"
  "Simple mode to edit Haml.

\\{haml-mode-map}"
  (set-syntax-table haml-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'haml-indent-line)
  (set (make-local-variable 'font-lock-defaults)
       '((haml-font-lock-keywords-1)
         nil
         t)))

;; Indentation and electric keys

(defun haml-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp) 10
      (forward-line -1)
      (while (and (looking-at haml-blank-line-re)
                  (> (point) (point-min)))
        (forward-line -1))
      (+ (current-indentation)
         (if (or (looking-at haml-filter-re)
                 (looking-at haml-comment-re)
                 (looking-at haml-html-comment-re)
                 (looking-at haml-block-cont-re)
                 (looking-at haml-tag-nest-re)
                 (looking-at haml-block-re))
             haml-indent-offset 0)))))

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
      (funcall haml-backspace-function arg)
    (let ((ci (current-column)))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to (* (/ (- ci (* arg haml-indent-offset))
                       haml-indent-offset)
                    haml-indent-offset)))))

;; Setup/Activation

(defun haml-mode-version ()
  "Diplay version of `haml-mode'."
  (interactive)
  (message "haml-mode %s" haml-mode-version)
  haml-mode-version)

(provide 'haml-mode)

(unless (find-if
         #'(lambda(it) (string= it "\\.haml\\'"))
         (mapcar 'car auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode)))

;;; haml-mode.el ends here
