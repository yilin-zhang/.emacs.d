;;; init-dired.el --- Configurations for dired mode. -*- lexical-binding: t -*-

;;; Commentary:
;; Configure Dired file management and related navigation behavior.

;;; Code:

(use-package dired
  :ensure nil
  :preface
  ;; Display file sizes in "human-readable" format.
  (defconst yilin/dired-portable-switches "-alh"
    "Listing switches understood by any ls, GNU or BSD.")
  (defconst yilin/dired-gnu-switches
    (concat yilin/dired-portable-switches " -v --group-directories-first")
    "Listing switches for GNU ls.
`-v' sorts `file2' before `file10'.")

  (defun yilin/dired-drop-gnu-switches-maybe ()
    "Use portable listing switches on remote hosts and with `ls-lisp'."
    (when (or (file-remote-p default-directory)
              (and (boundp 'ls-lisp-use-insert-directory-program)
                   (not ls-lisp-use-insert-directory-program)))
      (setq-local dired-actual-switches yilin/dired-portable-switches)))
  :hook (dired-mode . yilin/dired-drop-gnu-switches-maybe)
  :custom
  ;; Revert on visiting if the directory has changed on disk.
  (dired-auto-revert-buffer #'dired-buffer-stale-p)
  (dired-do-revert-buffer t)
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Offer to create missing parent directories.
  (dired-create-destination-dirs 'ask)
  ;; Making deleted files go to the trash can
  (delete-by-moving-to-trash t)
  ;; Suggest the directory of another Dired window as the target.
  (dired-dwim-target t)
  :config
  ;; The ls on macOS doesn't support `--dired', so use GNU ls (gls, from
  ;; `brew install coreutils') if there is one.  Also look in the Homebrew
  ;; directories, which may not be on `exec-path' yet.
  (if (not (eq system-type 'darwin))
      (setopt dired-listing-switches yilin/dired-gnu-switches)
    (let ((gls (or (executable-find "gls")
                   (seq-find #'file-executable-p
                             '("/opt/homebrew/bin/gls" "/usr/local/bin/gls")))))
      (if gls
          (setopt insert-directory-program gls
                  dired-listing-switches yilin/dired-gnu-switches)
        (setopt dired-use-ls-dired nil
                dired-listing-switches yilin/dired-portable-switches)))))

(use-package async
  :custom
  (dired-async-message-function
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
