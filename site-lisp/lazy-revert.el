;;; lazy-revert.el --- Lazy auto-revert buffers  -*- lexical-binding: t; -*-

;;; Commentary:

;; An alternative auto-revert solution extracted from Doom Emacs.

;;; Code:

(require 'cl-lib)
(require 'autorevert)

(defgroup lazy-revert nil
  "Auto save file when emacs idle."
  :group 'lazy-revert)

;;;###autoload
(defun lazy-revert--visible-buffers (&optional buffer-list)
  "Return a list of visible buffers (i.e. not buried)."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list)))))
    (if buffer-list
        (cl-delete-if (lambda (b) (memq b buffer-list))
                      buffers)
      (delete-dups buffers))))

(defun lazy-revert--auto-revert-buffer ()
  "Auto revert current buffer, if necessary."
  (unless (or auto-revert-mode (active-minibuffer-window))
    (let ((auto-revert-mode t))
      (auto-revert-handler))))

(defun lazy-revert--auto-revert-buffers ()
  "Auto revert stale buffers in visible windows, if necessary."
  (dolist (buf (lazy-revert--visible-buffers))
    (with-current-buffer buf
      (lazy-revert--auto-revert-buffer))))

(defun lazy-revert--run (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (lazy-revert--auto-revert-buffers)))

(defun lazy-revert--run-switch-window-or-frame (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (unless (equal (old-selected-frame) (selected-frame))
      (lazy-revert--auto-revert-buffers))
    (unless (or (minibufferp)
                (equal (old-selected-window) (minibuffer-window)))
      (lazy-revert--auto-revert-buffers))))

(defun lazy-revert--setup ()
  (add-hook 'window-selection-change-functions #'lazy-revert--run-switch-window-or-frame)
  (add-hook 'window-buffer-change-functions #'lazy-revert--run)
  (add-function :after after-focus-change-function #'lazy-revert--run-switch-window-or-frame)
  (add-hook 'after-save-hook #'lazy-revert--run))

(defun lazy-revert--cleanup()
  (remove-hook 'window-selection-change-functions #'lazy-revert--run-switch-window-or-frame)
  (remove-hook 'window-buffer-change-functions #'lazy-revert--run)
  (remove-function after-focus-change-function #'lazy-revert--run-switch-window-or-frame)
  (remove-hook 'after-save-hook #'lazy-revert--run))

;;;###autoload
(define-minor-mode lazy-revert-mode
  "Toggle `lazy-revert-mode' on or off."
  :global t
  :group 'lazy-revert
  (if lazy-revert-mode
      (lazy-revert--setup)
    (lazy-revert--cleanup)))

(provide 'lazy-revert)
;;; lazy-revert.el ends here

