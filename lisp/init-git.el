;; init-git.el --- Configurations for git. -*- lexical-binding: t -*-

;; Magit
(use-package magit
  :commands (transient-insert-suffix magit-status)
  :bind ("C-x g" . magit-status)
  ;; turn off auto revert since global-auto-revert-mode is enabled
  :config (magit-auto-revert-mode -1))

(provide 'init-git)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-git.el ends here
