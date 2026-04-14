;;; indent-shift.el --- Vim-like shift-left/right for regions -*- lexical-binding: t -*-

;;; Commentary:

;; Vim-style `>>' / `<<' for Emacs.  Shifts the leading indentation of
;; every line in the region (or current line) by a width that tracks
;; the current major mode's own offset variable
;; (e.g. `python-indent-offset', `js-indent-level').
;;
;; Extending to a new language is a one-line edit to
;; `indent-shift-width-alist'; the commands themselves stay
;; mode-agnostic.
;;
;; Design is lifted from evil's `evil-shift-right' (per-line
;; `current-indentation' + `indent-to', with optional rounding) but
;; pulls the shift width from the mode's native offset variable
;; instead of a single global knob.

;;; Code:

(require 'cl-lib)

(defcustom indent-shift-width-alist
  '((python-base-mode . python-indent-offset)
    (js-base-mode     . js-indent-level)
    (json-mode        . js-indent-level)
    (json-ts-mode     . js-indent-level)
    (css-base-mode    . css-indent-offset)
    (ruby-base-mode   . ruby-indent-level)
    (yaml-mode        . yaml-indent-offset)
    (yaml-ts-mode     . yaml-indent-offset)
    (lua-mode         . lua-indent-level)
    (pico8-mode       . lua-indent-level))
  "Alist mapping a major mode to the variable holding its indent offset.
Matching uses `derived-mode-p', so a base mode entry covers its
classic and tree-sitter derivatives in one line.  The first
matching entry wins; falls back to `tab-width' when no entry
matches or the named variable is unbound."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'indent)

(defcustom indent-shift-round t
  "When non-nil, snap the new indent to the nearest multiple of the shift width.
Mirrors `evil-shift-round': shifting a line indented 3 columns by a
width of 4 goes to 4, not 7."
  :type 'boolean
  :group 'indent)

(defcustom indent-shift-preserve-empty nil
  "When non-nil, also indent lines that contain only whitespace."
  :type 'boolean
  :group 'indent)

(defun indent-shift--width ()
  "Return the shift width for the current buffer."
  (or (cl-some (lambda (entry)
                 (and (derived-mode-p (car entry))
                      (boundp (cdr entry))
                      (symbol-value (cdr entry))))
               indent-shift-width-alist)
      tab-width))

(defun indent-shift--region-bounds ()
  "Return (BEG . END) for the region, or the current line if no region."
  (if (use-region-p)
      (cons (save-excursion (goto-char (region-beginning))
                            (line-beginning-position))
            (save-excursion (goto-char (region-end))
                            (if (bolp) (point) (line-beginning-position 2))))
    (cons (line-beginning-position) (line-beginning-position 2))))

(defun indent-shift--shift (beg end count)
  "Shift each line in [BEG, END) by COUNT multiples of the shift width."
  (let ((width (indent-shift--width))
        (end-marker (copy-marker end)))
    (save-excursion
      (goto-char beg)
      (while (< (point) end-marker)
        (let* ((indent (current-indentation))
               (new-indent
                (max 0
                     (if (not indent-shift-round)
                         (+ indent (* count width))
                       (* (+ (/ indent width)
                             count
                             (cond
                              ((> count 0) 0)
                              ((zerop (mod indent width)) 0)
                              (t 1)))
                          width)))))
          (when (or indent-shift-preserve-empty
                    (save-excursion (skip-chars-forward " \t") (not (eolp))))
            (indent-line-to new-indent)))
        (forward-line 1)))
    (set-marker end-marker nil)))

;;;###autoload
(defun indent-shift-right (&optional count)
  "Shift the region (or current line) right by COUNT indent steps."
  (interactive "p")
  (let ((bounds (indent-shift--region-bounds)))
    (indent-shift--shift (car bounds) (cdr bounds) (or count 1))))

;;;###autoload
(defun indent-shift-left (&optional count)
  "Shift the region (or current line) left by COUNT indent steps."
  (interactive "p")
  (indent-shift-right (- (or count 1))))

(provide 'indent-shift)
;;; indent-shift.el ends here
