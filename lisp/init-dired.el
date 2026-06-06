;; init-dired.el --- Configurations for dired mode. -*- lexical-binding: t -*-

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
  ;; macOS ships BSD ls, which doesn't understand `--dired'; that's what
  ;; triggers the "ls does not support --dired" warning. Prefer GNU ls
  ;; (gls, from `brew install coreutils') for full dired support. Detect
  ;; it robustly: `executable-find' can fail here if this runs before
  ;; exec-path-from-shell has populated `exec-path', so also probe the
  ;; standard Homebrew locations directly. If there's no gls at all, tell
  ;; dired to stop trying `--dired' so the warning goes away.
  (when (eq system-type 'darwin)
    (let ((gls (or (executable-find "gls")
                   (seq-find #'file-executable-p
                             '("/opt/homebrew/bin/gls" "/usr/local/bin/gls")))))
      (if gls
          (setq insert-directory-program gls)
        (setq dired-use-ls-dired nil)))))

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

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package nerd-icons-dired
  :after dired
  :hook dired-mode)

(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
