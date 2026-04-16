;; init-git.el --- Configurations for git. -*- lexical-binding: t -*-

;; Magit
(use-package magit
  :commands (transient-insert-suffix magit-status)
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-auto-revert-mode nil))

(use-package blamer
  :commands blamer-mode
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground ,(face-foreground 'warning)
                   :background unspecified
                   :height 140
                   :italic t))))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :preface
  (defun yilin/diff-hl-dired-mode-unless-remote ()
    (unless (file-remote-p default-directory)
      (diff-hl-dired-mode)))
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (diff-hl-mode . diff-hl-flydiff-mode)
         (dired-mode . yilin/diff-hl-dired-mode-unless-remote))
  :config
  (setq diff-hl-update-async t)
  (set-face-attribute 'diff-hl-change nil
                      :background 'unspecified
                      :foreground (face-foreground 'warning))
  (set-face-attribute 'diff-hl-insert nil
                      :background 'unspecified
                      :foreground (face-foreground 'success))
  (set-face-attribute 'diff-hl-delete nil
                      :background 'unspecified
                      :foreground (face-foreground 'error))
  (with-no-warnings
    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

(provide 'init-git)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-git.el ends here
