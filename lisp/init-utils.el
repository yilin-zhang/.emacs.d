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
;;                            Dashbord
;; --------------------------------------------------------------
(use-package dashboard
  :init
  ;; add instruction
  (setq initial-scratch-message
        (concat initial-scratch-message
                ";; Press <f6> to open the dashboard\n\n"))
  ;; layout
  (setq dashboard-center-content t)
  ;; icons
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  :bind
  ("<f6>" . dashboard-open)
  :custom
  ;; minimalist layout
  (dashboard-startupify-list '(dashboard-insert-items))
  (dashboard-vertically-center-content t)
  ;; only show names, not paths
  (dashboard-agenda-prefix-format " %i %-12:c%?-12t% s %b") ; same as the agenda format in `org-agenad-prefix-format'
  (dashboard-agenda-time-string-format "%m-%d %a") ; add an abbreviated weekday name
  (dashboard-bookmarks-item-format "%s") ; no need to show file paths
  (dashboard-recentf-show-base t)
  (dashboard-recentf-item-format "%s") ; only show file names, not whole paths
  ;; limited items
  (dashboard-items '((bookmarks . 10)
                     (projects  . 10)
                     (agenda    . 5)
                     (recents   . 5)
                     (registers . 5))))

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
  :vc (:url "https://github.com/manateelazycat/auto-save.git")
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
  :load-path yilin/site-lisp-directory
  :commands (spamemo-add-word
             spamemo-review spamemo-reload-deck
             spamemo-open-vocab-file))

(use-package spamemo-calendar
  :ensure nil
  :load-path yilin/site-lisp-directory
  :hook (after-init . spamemo-calendar-mode))

(defun yilin/lookup-thesaurus (&optional arg)
  "Look up the word on an online thesaurus.
If a region is active, use the text in that region (whitespaces stripped).
If no region is active, use the word at point.
If no word is at point, prompt for a word, using prefix arg as default if provided."
  (interactive "P")
  (let ((word
         (cond
          ;; If region is active, use the region text and strip whitespace
          ((use-region-p)
           (string-trim (buffer-substring-no-properties (region-beginning) (region-end))))
          ;; If there's a word at point, use it
          ((thing-at-point 'word)
           (thing-at-point 'word t))
          ;; Otherwise, prompt for a word
          (t
           (read-string "Word for thesaurus: " (when arg (format "%s" arg)))))))
    (browse-url (format "https://www.merriam-webster.com/thesaurus/%s" word))))

;; --------------------------------------------------------------
;;                            Calc
;; --------------------------------------------------------------
(use-package literate-calc-mode)

;; --------------------------------------------------------------
;;                            Sys Tools
;; --------------------------------------------------------------
(use-package trashcat
  :ensure nil
  :load-path yilin/site-lisp-directory
  :commands trashcat)

;; --------------------------------------------------------------
;;                         Terminal
;; --------------------------------------------------------------
(use-package vterm
  :after meow
  :commands vterm
  :hook
  (vterm-mode . (lambda () (meow-mode -1)))
  (vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
  )

(use-package multi-vterm
  :commands multi-vterm
  :after vterm)

;; --------------------------------------------------------------
;;                            Custom
;; --------------------------------------------------------------

;; **************************************************************
;; Open in finder and terminal (Mac)
;; **************************************************************

(defun yilin/expand-and-quote-default-directory ()
  (shell-quote-argument (expand-file-name default-directory)))

(defun yilin/open-with-terminal ()
  "Open the current dir in a new iTerm window."
  (interactive)
  (shell-command (concat "open -a iTerm.app " (yilin/expand-and-quote-default-directory))))

(defun yilin/open-with-finder-or-default-app ()
  "Open the current dir with Finder or open the current file with a default app"
  (interactive)
  (let ((dired-file (condition-case nil
                        (dired-get-filename)
                      (error nil))))
    (if dired-file
        (shell-command (concat "open " (shell-quote-argument dired-file)))
      (shell-command (concat "open " (yilin/expand-and-quote-default-directory))))))

(use-package emacs
  :after meow
  :config
  (meow-leader-define-key
   '("t" . yilin/open-with-terminal)
   '("e" . yilin/open-with-finder-or-default-app)))

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
