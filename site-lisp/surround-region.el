;;; surround-region.el --- Surround region with a character -*- lexical-binding: t -*-

;;; Commentary:

;; Commands to surround the non-whitespace content of the selected
;; region with a character.  `yilin/surround-region' is the generic
;; entry point; the `yilin/surround-region-*' wrappers cover common
;; characters and are collected in the prefix keymap
;; `surround-region-map' under their literal key.

;;; Code:

(defun yilin/surround-region (char)
  "Surround the selected region's non-whitespace content with CHAR.
Supports *, =, +, / and properly pairs (, [, {."
  (interactive "cEnter character to surround with: ")
  (let* ((pairs '((?\( . ?\))
                  (?\[ . ?\])
                  (?\{ . ?\})
                  (?\< . ?\>)))
         (left-char char)
         (right-char (or (cdr (assoc char pairs)) char)))
    (if (use-region-p)
        (let ((beg (region-beginning))
              (end (region-end)))
          (save-excursion
            (save-restriction
              (narrow-to-region beg end)
              (goto-char (point-min))
              (skip-chars-forward " \t\n")
              (let ((left-pos (point)))
                (goto-char (point-max))
                (skip-chars-backward " \t\n")
                (let ((right-pos (point)))
                  ;; Insert in reverse to preserve positions
                  (goto-char right-pos)
                  (insert right-char)
                  (goto-char left-pos)
                  (insert left-char))))))
      (message "No region selected"))))

(defun yilin/surround-region-equal ()
  (interactive)
  (yilin/surround-region ?=))

(defun yilin/surround-region-plus ()
  (interactive)
  (yilin/surround-region ?+))

(defun yilin/surround-region-asterisk ()
  (interactive)
  (yilin/surround-region ?*))

(defun yilin/surround-region-dash ()
  (interactive)
  (yilin/surround-region ?-))

(defun yilin/surround-region-paren ()
  (interactive)
  (yilin/surround-region ?\())

(defun yilin/surround-region-bracket ()
  (interactive)
  (yilin/surround-region ?\[))

(defun yilin/surround-region-curly ()
  (interactive)
  (yilin/surround-region ?\{))

(defun yilin/surround-region-angle ()
  (interactive)
  (yilin/surround-region ?\<))

(defun yilin/surround-region-quote ()
  (interactive)
  (yilin/surround-region ?\'))

(defun yilin/surround-region-dquote ()
  (interactive)
  (yilin/surround-region ?\"))

(defun yilin/surround-region-slash ()
  (interactive)
  (yilin/surround-region ?/))

(defun yilin/surround-region-underscore ()
  (interactive)
  (yilin/surround-region ?_))

(defun yilin/surround-region-tilde ()
  (interactive)
  (yilin/surround-region ?~))

(defvar surround-region-map
  (let ((map (make-sparse-keymap)))
    (define-key map "=" #'yilin/surround-region-equal)
    (define-key map "+" #'yilin/surround-region-plus)
    (define-key map "*" #'yilin/surround-region-asterisk)
    (define-key map "-" #'yilin/surround-region-dash)
    (define-key map "(" #'yilin/surround-region-paren)
    (define-key map ")" #'yilin/surround-region-paren)
    (define-key map "[" #'yilin/surround-region-bracket)
    (define-key map "]" #'yilin/surround-region-bracket)
    (define-key map "{" #'yilin/surround-region-curly)
    (define-key map "}" #'yilin/surround-region-curly)
    (define-key map "<" #'yilin/surround-region-angle)
    (define-key map ">" #'yilin/surround-region-angle)
    (define-key map "'" #'yilin/surround-region-quote)
    (define-key map "\"" #'yilin/surround-region-dquote)
    (define-key map "/" #'yilin/surround-region-slash)
    (define-key map "_" #'yilin/surround-region-underscore)
    (define-key map "~" #'yilin/surround-region-tilde)
    map)
  "Prefix keymap for `yilin/surround-region' wrappers.")
(fset 'surround-region-map surround-region-map)

(provide 'surround-region)
;;; surround-region.el ends here
