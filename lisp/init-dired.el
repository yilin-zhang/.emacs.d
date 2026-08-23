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
    "Listing switches for GNU coreutils ls.
`-v' sorts `file2' before `file10' instead of after it, and
`--group-directories-first' floats directories to the top.")

  (defun yilin/dired-drop-gnu-switches-maybe ()
    "Fall back to portable listing switches where GNU ls isn't what runs.
Over TRAMP the remote host may well be running BSD ls, and on Windows
`ls-lisp' emulates the listing in Lisp; passing GNU-only switches there
yields a blank dired buffer or unsortable entries."
    (when (or (file-remote-p default-directory)
              (and (boundp 'ls-lisp-use-insert-directory-program)
                   (not ls-lisp-use-insert-directory-program)))
      (setq-local dired-actual-switches yilin/dired-portable-switches)))
  :hook (dired-mode . yilin/dired-drop-gnu-switches-maybe)
  :custom
  ;; Revert only when the directory has actually changed on disk, rather
  ;; than on every visit.
  (dired-auto-revert-buffer #'dired-buffer-stale-p)
  (dired-do-revert-buffer t)
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Offer to create missing parent directories rather than failing.
  (dired-create-destination-dirs 'ask)
  ;; Making deleted files go to the trash can
  (delete-by-moving-to-trash t)
  ;; make copy files under dired mode easier
  ;; it provides a dir choice which is the path of another buffer
  (dired-dwim-target t)
  :config
  ;; macOS ships BSD ls, which doesn't understand `--dired'; that's what
  ;; triggers the "ls does not support --dired" warning. Prefer GNU ls
  ;; (gls, from `brew install coreutils') for full dired support. Detect
  ;; it robustly: `executable-find' can fail here if this runs before
  ;; exec-path-from-shell has populated `exec-path', so also probe the
  ;; standard Homebrew locations directly. If there's no gls at all, tell
  ;; dired to stop trying `--dired' so the warning goes away, and stick to
  ;; the switches BSD ls understands.
  (if (not (eq system-type 'darwin))
      (setq dired-listing-switches yilin/dired-gnu-switches)
    (let ((gls (or (executable-find "gls")
                   (seq-find #'file-executable-p
                             '("/opt/homebrew/bin/gls" "/usr/local/bin/gls")))))
      (if gls
          (setq insert-directory-program gls
                dired-listing-switches yilin/dired-gnu-switches)
        (setq dired-use-ls-dired nil
              dired-listing-switches yilin/dired-portable-switches)))))

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
