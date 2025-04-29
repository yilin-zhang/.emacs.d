;; init-utils.el --- Utility configurations. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                         File Tree
;; --------------------------------------------------------------
(use-package treemacs
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-git-mode)
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  :bind (([f8]        . treemacs)
         ("M-0"       . treemacs-select-window)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t b"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-width                   30
        treemacs-no-png-images           nil)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-project-follow-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-nerd-icons
  :demand t
  :after (treemacs nerd-icons)
  :custom-face
  (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
  (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
  :config (treemacs-load-theme "nerd-icons"))

(use-package treemacs-magit
  :after (treemacs magit)
  :hook ((magit-post-commit
          git-commit-post-finish
          magit-post-stage
          magit-post-unstage)
         . treemacs-magit--schedule-update))

;; --------------------------------------------------------------
;;                         Better Writing
;; --------------------------------------------------------------
(use-package olivetti
  :bind
  ("<f7>" . olivetti-mode)
  :hook
  (org-mode . olivetti-mode))

;; --------------------------------------------------------------
;;                         Better Doc
;; --------------------------------------------------------------
(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command)
  ("C-h d" . helpful-at-point)
  ("C-h F" . helpful-function))

;; --------------------------------------------------------------
;;                        Note Taking
;; --------------------------------------------------------------
(use-package denote)

(use-package annotate)

;; --------------------------------------------------------------
;;                           Project
;; --------------------------------------------------------------
(defun yilin/ibuffer-project-hook ()
  (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
  (unless (eq ibuffer-sorting-mode 'project-file-relative)
    (ibuffer-do-sort-by-project-file-relative)))

(use-package ibuffer-project
  :hook (ibuffer . yilin/ibuffer-project-hook))

(use-package eyebrowse
  :hook (after-init . eyebrowse-mode))

;; --------------------------------------------------------------
;;                           Info
;; --------------------------------------------------------------
(use-package info
  :hook (Info-mode . yilin/set-variable-pitch))

;; --------------------------------------------------------------
;;                           File
;; --------------------------------------------------------------
(use-package auto-save
  :ensure nil
  :quelpa (auto-save :repo "manateelazycat/auto-save"
                     :fetcher github)
  :hook (after-init . auto-save-enable)
  :config
  (setq auto-save-silent t)             ; quietly save
  ;; (setq auto-save-delete-trailing-whitespace t) ; automatically delete spaces at the end of the line when saving
  (setq auto-save-disable-predicates
        '((lambda ()
            (string-suffix-p
             "gpg"
             (file-name-extension (buffer-name)) t)))))

;; --------------------------------------------------------------
;;                            Vocab
;; --------------------------------------------------------------
(use-package spamemo
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/"
  :commands
  (spamemo-add-word spamemo-review spamemo-reload-deck))

;; --------------------------------------------------------------
;;                            Calc
;; --------------------------------------------------------------
(use-package literate-calc-mode)

;; --------------------------------------------------------------
;;                            Custom
;; --------------------------------------------------------------

;; **************************************************************
;; Open in finder and terminal (Mac)
;; **************************************************************
(defun yilin/open-in-finder ()
  "Open the current dir in Finder."
  (interactive)
  (shell-command (format "open \"%s\"" default-directory)))

(defun yilin/open-in-terminal ()
  "Open the current dir in a new terminal window."
  (interactive)
  (shell-command (format "open -a iTerm.app \"%s\"" default-directory)))

(meow-leader-define-key
 '("t" . yilin/open-in-terminal)
 '("e" . yilin/open-in-finder))

;; **************************************************************
;; Copy file path and name
;; **************************************************************
(defun yilin/copy-file-path ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (let ((filename (yilin/-copy-file-path)))
    (when filename
      (kill-new filename)
      (message "Copied buffer file path '%s' to the clipboard." filename))))

(defun yilin/copy-file-name ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (let ((filename (file-name-base (yilin/-copy-file-path))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))


(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
