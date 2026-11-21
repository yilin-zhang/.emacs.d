;;; diff-hl-refresh.el --- Keep diff-hl's gutter current  -*- lexical-binding: t; -*-

;;; Commentary:

;; diff-hl updates its gutter on save and on revert.  Two global modes
;; cover the rest:
;;
;; - `diff-hl-refresh-mode' watches the git directory of each repository
;;   shown in a window.  When a git command run outside Emacs, such as a
;;   commit from a shell, changes the index or HEAD, it refreshes the
;;   shown buffers.
;; - `diff-hl-refresh-on-select-mode' refreshes the buffer of each newly
;;   selected window, in place of `diff-hl-flydiff-mode'.
;;
;; `diff-hl-refresh-maybe' can be hooked wherever else a refresh helps,
;; such as on leaving an insert state.

;;; Code:

(require 'diff-hl)
(require 'filenotify)
(require 'vc-git)

;;;; Refresh on select

(defvar-local diff-hl-refresh--last-state nil)

(defun diff-hl-refresh-maybe ()
  "Refresh the current buffer's gutter unless nothing can have changed.
Skip it while point and the file's cached VC state stay the same, so
that frequent callers don't spawn a git process each time."
  (when-let* (((or (bound-and-true-p diff-hl-mode)
                   (bound-and-true-p diff-hl-dir-mode)))
              ;; Refreshing over TRAMP is far too slow to do this often.
              ((not (file-remote-p default-directory)))
              (file (buffer-file-name (buffer-base-buffer)))
              ((not (equal (cons (point) diff-hl-refresh--last-state)
                           (setq diff-hl-refresh--last-state
                                 (cons (point)
                                       (copy-sequence
                                        (symbol-plist
                                         (intern (expand-file-name file)
                                                 vc-file-prop-obarray)))))))))
    (ignore (diff-hl-update))))

(defun diff-hl-refresh--on-select (&optional frame-or-window)
  "Refresh the gutter of the newly selected window.
`window-selection-change-functions' passes the frame for the global hook
value and the window for a window-local one, and neither guarantees the
matching buffer is current, so resolve it explicitly."
  (let ((win (cond ((windowp frame-or-window) frame-or-window)
                   ((framep frame-or-window)
                    (frame-selected-window frame-or-window))
                   (t (selected-window)))))
    (when (window-live-p win)
      (with-current-buffer (window-buffer win)
        (diff-hl-refresh-maybe)))))

;;;###autoload
(define-minor-mode diff-hl-refresh-on-select-mode
  "Refresh the gutter of each newly selected window."
  :global t
  :group 'diff-hl
  (if diff-hl-refresh-on-select-mode
      (add-hook 'window-selection-change-functions #'diff-hl-refresh--on-select)
    (remove-hook 'window-selection-change-functions #'diff-hl-refresh--on-select)))

;;;; Refresh on git changes

(defvar diff-hl-refresh--watches nil
  "Alist of (GIT-DIR . WATCH) for the repositories shown in windows.")

(defvar diff-hl-refresh--timer nil)

(defun diff-hl-refresh--git-dir ()
  "Return the git directory of the current buffer's file, or nil.
Linked worktrees, whose .git is a file, are left out."
  (when-let* (((bound-and-true-p diff-hl-mode))
              (file buffer-file-name)
              ((not (file-remote-p file)))
              ((eq (vc-backend file) 'Git))
              (dir (expand-file-name ".git" (vc-git-root file)))
              ((file-directory-p dir)))
    dir))

(defun diff-hl-refresh--shown-buffers ()
  "Return the buffers shown in windows whose file is in a git repository."
  (seq-filter (lambda (buf)
                (with-current-buffer buf (diff-hl-refresh--git-dir)))
              (delete-dups (mapcar #'window-buffer
                                   (window-list-1 nil 'nomini t)))))

(defun diff-hl-refresh--shown ()
  "Refresh the VC state and gutter of the buffers shown in windows."
  (dolist (buf (diff-hl-refresh--shown-buffers))
    (with-current-buffer buf
      (vc-state-refresh buffer-file-name 'Git)
      (diff-hl-update))))

(defun diff-hl-refresh--git-changed (event)
  "Refresh the shown buffers shortly after EVENT touches the index or HEAD.
One git command writes several files, so wait for it to finish."
  (when (member (file-name-nondirectory (or (nth 3 event) (nth 2 event)))
                '("index" "HEAD"))
    (when (timerp diff-hl-refresh--timer)
      (cancel-timer diff-hl-refresh--timer))
    (setq diff-hl-refresh--timer
          (run-with-timer 0.3 nil #'diff-hl-refresh--shown))))

(defun diff-hl-refresh--sync-watches (&rest _)
  "Watch exactly the git directories of the buffers shown in windows."
  (let ((dirs (delete-dups
               (mapcar (lambda (buf)
                         (with-current-buffer buf (diff-hl-refresh--git-dir)))
                       (diff-hl-refresh--shown-buffers)))))
    (pcase-dolist (`(,dir . ,watch) diff-hl-refresh--watches)
      (unless (member dir dirs)
        (file-notify-rm-watch watch)))
    (setq diff-hl-refresh--watches
          (seq-keep
           (lambda (dir)
             (let ((watch (alist-get dir diff-hl-refresh--watches
                                     nil nil #'equal)))
               (when-let* ((watch (if (and watch (file-notify-valid-p watch))
                                      watch
                                    (ignore-errors
                                      (file-notify-add-watch
                                       dir '(change)
                                       #'diff-hl-refresh--git-changed)))))
                 (cons dir watch))))
           dirs))))

;;;###autoload
(define-minor-mode diff-hl-refresh-mode
  "Refresh the shown buffers' gutters when their repository changes."
  :global t
  :group 'diff-hl
  (if diff-hl-refresh-mode
      (progn
        (add-hook 'window-buffer-change-functions #'diff-hl-refresh--sync-watches)
        (diff-hl-refresh--sync-watches))
    (remove-hook 'window-buffer-change-functions #'diff-hl-refresh--sync-watches)
    (pcase-dolist (`(_ . ,watch) diff-hl-refresh--watches)
      (file-notify-rm-watch watch))
    (setq diff-hl-refresh--watches nil)))

(provide 'diff-hl-refresh)
;;; diff-hl-refresh.el ends here
