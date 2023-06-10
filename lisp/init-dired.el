;; init-basic.el --- Configurations for dired mode. -*- lexical-binding: t -*-

(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-do-revert-buffer t)
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Making deleted files go to the trash can
  (delete-by-moving-to-trash t)
  ;; make copy files under dired mode easier
  ;; it provides a dir choice which is the path of another buffer
  (dired-dwim-target t)
  ;; display file sizes in “human-readable” format
  (dired-listing-switches "-alh")
  :config
  ;; support --dired
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls")))

(use-package dired-rsync
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)))

(use-package nerd-icons-dired
  :after dired
  :hook dired-mode)

(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
