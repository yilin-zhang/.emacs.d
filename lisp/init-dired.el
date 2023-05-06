;; init-basic.el --- Configurations for dired mode. -*- lexical-binding: t -*-

(use-package dired
  :ensure nil
  :custom
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
  )

(use-package async
  :init
  (setq dired-async-message-function
        (lambda (text face &rest args)
          "Notify end of operation in `mode-line'."
          (message (propertize
                    (if args
                        (apply #'format text args)
                      text)
                    'face face))))
  :after dired
  :hook (dired-mode . dired-async-mode))

;; support --dired
(use-package ls-lisp
  :ensure nil
  :demand
  :custom (ls-lisp-use-insert-directory-program nil))

(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode))

(use-package nerd-icons-dired
  :after dired
  :hook dired-mode)

(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
