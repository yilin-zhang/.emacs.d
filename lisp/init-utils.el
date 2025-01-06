;; init-utils.el --- Utility configurations. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                         Terminal
;; --------------------------------------------------------------
(use-package vterm
  :commands vterm
  :hook (vterm-mode . (lambda ()
                        (setq-local global-hl-line-mode nil)
                        (meow-mode -1))))

(use-package multi-vterm
  :commands multi-vterm
  :after vterm)

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
;;                           Feed
;; --------------------------------------------------------------
(use-package elfeed
  :bind ("C-x w" . elfeed)
  (:map elfeed-search-mode-map
        ("j" . next-line)
        ("k" . previous-line)
        ("h" . backward-char)
        ("l" . forward-char))
  :config
  (setq elfeed-search-filter "@6-months-ago"))

(use-package info
  :hook (Info-mode . yilin/set-variable-pitch))

;; --------------------------------------------------------------
;;                           LLM
;; --------------------------------------------------------------
(use-package gptel
  :bind (:map gptel-mode-map
              ("C-<return>" . yilin/gptel-add-prompt))
  :hook
  (gptel-mode . visual-line-mode)
  (gptel-mode . (lambda () (auto-fill-mode -1)))
  (gptel-mode . yilin/gptel--setup-default-directive)
  :config
  (defun yilin/gptel-add-prompt ()
    (interactive)
    (gptel-prompt-string)
    (let ((prefix (cdr (assoc major-mode gptel-prompt-prefix-alist)))
          (filling (if (and (equal (point) 0) (eolp))
                       ""
                     "\n\n")))
      (end-of-line)
      (insert (concat filling prefix))))

  (defun yilin/gptel-select-directive ()
    "Select the role of ChatGPT"
    (interactive)
    (let ((option
           (intern (completing-read "AI's role: " gptel-directives))))
      (setq gptel--system-message
            (alist-get option gptel-directives))
      (message "Selected AI's role: %s" option)))

  (defun yilin/gptel--setup-default-directive ()
    (setq gptel--system-message (alist-get 'teacher gptel-directives)))

  (defun yilin/gptel--get-api-key ()
    (with-temp-buffer
      (insert-file-contents "~/.emacs.d/custom/gptel-api-key")
      (string-trim (buffer-string))))

  (setq gptel-default-mode 'markdown-mode)
  (setq gptel-api-key (yilin/gptel--get-api-key))
  ;; custom ai roles
  (dolist (directive '((teacher . "You are a large language model\
 and a patient teacher, who can break down complex concepts and\
 terms into easy-to-follow text.")))
    (add-to-list 'gptel-directives directive))
  )

;; --------------------------------------------------------------
;;                        Fancy Stuff
;; --------------------------------------------------------------
(use-package dictionary
  :ensure nil
  :config
  (setq dictionary-use-single-buffer t)
  (setq dictionary-server "dict.org"))

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

(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
