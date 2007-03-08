;;; haml-mode.el -- Major mode for editing Haml files
;;; Version 0.0.1
;;; Written by Nathan Weizenbaum

;;; Because Haml's indentation schema is similar
;;; to that of YAML and Python, many indentation-related
;;; functions are similar to those in yaml-mode and python-mode.

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

;; Helper Functions

(defun string-* (str i)
  (if (= i 0) ""
    (concat str (string-* str (- i 1)))))

(defun hre (str)
  "Prepends a Haml-tab-matching regexp to str."
  (concat "^\\(" (string-* " " haml-indent-offset) "\\)*" str))

;; Constants

(defconst haml-mode-version "0.0.1" "Version of `haml-mode.'")

(defconst haml-blank-line-re "^[ \t]*$"
  "Regexp matching a line containing only whitespace.")

(defconst haml-tag-re (hre "[%\\.#][^ \t]*\\({.*}\\)?\\(\\[.*\\]\\)?.?[ \t]*$")
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

(defvar haml-mode-map ()
  "Keymap used in `haml-mode' buffers.")
(if haml-mode-map
    nil
  (setq haml-mode-map (make-sparse-keymap))
  (define-key haml-mode-map [backspace] 'haml-electric-backspace)
  (define-key haml-mode-map "\C-?" 'haml-electric-backspace)
  (define-key haml-mode-map "\C-a" 'haml-electric-backspace)
  (define-key haml-mode-map "\C-j" 'newline-and-indent))

;(defvar haml-mode-syntax-table nil
;  "Syntax table in use in haml-mode buffers.")
;(if haml-mode-syntax-table
;    nil
;  (setq haml-mode-syntax-table (make-syntax-table))
;  ...
;  )

(define-derived-mode haml-mode fundamental-mode "Haml"
  "Simple mode to edit Haml.

\\{haml-mode-map}"
  (set (make-local-variable 'indent-line-function) 'haml-indent-line))
;  (set (make-local-variable 'font-lock-defaults)
;       '(haml-font-lock-keywords
;         nil nil nil nil
;         (font-lock-syntactic-keywords . haml-font-lock-syntactic-keywords))))

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
                 (looking-at haml-tag-re)
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

(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))

;;; haml-mode.el ends here
