;; init-basic.el --- Configurations for dired mode. -*- lexical-binding: t -*-

(require 'dired)

;; use kbd a to reuse a dired buffer
(put 'dired-find-alternate-file 'disabled nil)

;; Always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; Making deleted files go to the trash can
(setq delete-by-moving-to-trash t)

;; make copy files under dired mode easier
;; it provides a dir choice which is the path of another buffer
(setq dired-dwim-target t)

;; display file sizes in “human-readable” format
(setq dired-listing-switches "-alh")

;; use C-x C-j to open the current dir
(require 'dired-x)

(use-package diredfl
  :init (diredfl-global-mode 1))

(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
