;; init-git.el --- Configurations for git. -*- lexical-binding: t -*-

;; Magit
(use-package magit
  :commands (transient-insert-suffix magit-status)
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-auto-revert-mode nil))

(provide 'init-git)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-git.el ends here
