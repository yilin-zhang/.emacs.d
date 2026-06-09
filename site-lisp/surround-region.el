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

(defconst surround-region--specs
  '(("equal"      ?= "=")
    ("plus"       ?+ "+")
    ("asterisk"   ?* "*")
    ("dash"       ?- "-")
    ("paren"      ?\( "(" ")")
    ("bracket"    ?\[ "[" "]")
    ("curly"      ?\{ "{" "}")
    ("angle"      ?< "<" ">")
    ("quote"      ?' "'")
    ("dquote"     ?\" "\"")
    ("slash"      ?/ "/")
    ("underscore" ?_ "_")
    ("tilde"      ?~ "~"))
  "Wrapper specs: (NAME CHAR KEYS...).
Each spec defines the command `yilin/surround-region-NAME' that
surrounds the region with CHAR, bound to KEYS in
`surround-region-map'.")

(defvar surround-region-map
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,name ,char . ,keys) surround-region--specs)
      (let ((cmd (intern (concat "yilin/surround-region-" name))))
        (defalias cmd
          (lambda () (interactive) (yilin/surround-region char))
          (format "Surround the region with `%c'." char))
        (dolist (key keys)
          (define-key map key cmd))))
    map)
  "Prefix keymap for `yilin/surround-region' wrappers.")
(fset 'surround-region-map surround-region-map)

(provide 'surround-region)
;;; surround-region.el ends here
