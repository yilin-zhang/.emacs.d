;; init-utils.el --- Utility configurations. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                         Better Writting
;; --------------------------------------------------------------
(use-package writeroom-mode
  :bind
  ("<f6>" . global-writeroom-mode)
  ("<f7>" . writeroom-mode)
  :config
  (add-to-list 'writeroom-major-modes 'org-mode)
  (setq writeroom-width 90))

;; ;; --------------------------------------------------------------
;;                           File Tree
;; --------------------------------------------------------------
(use-package neotree
  ;; override evil-mode key bindings
  :bind
  ("<f8>" . neotree-toggle)
  :hook (neotree-mode . (lambda ()
                          (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                          (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
                          (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                          (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                          (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
                          (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
                          (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
                          (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
                          (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))
  :config
  (setq neo-smart-open t)
  ;; requires all-the-icons
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; --------------------------------------------------------------
;;                           Projectile
;; --------------------------------------------------------------
(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
              ("s-t" . projectile-find-file) ; `cmd-t' or `super-t'
              ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix "")
  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t)
  :config
  (projectile-update-mode-line)
  (setq projectile-completion-system 'ivy))

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
  :functions all-the-icons-octicon
  :hook ((ibuffer . (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic)))))
  :config
  (setq ibuffer-projectile-prefix
        (if (display-graphic-p)
            (concat
             (all-the-icons-octicon "file-directory"
                                    :face ibuffer-filter-group-name-face
                                    :v-adjust -0.04
                                    :height 1.1)
             " ")
          "Project: ")))

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
  (:map elfeed-show-mode-map
        ("j" . next-line)
        ("k" . previous-line)
        ("h" . backward-char)
        ("l" . forward-char))
  :config
  (setq elfeed-search-filter "@6-months-ago")
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs))

;; --------------------------------------------------------------
;;                           Fancy Stuff
;; --------------------------------------------------------------

;; try packages without actually install them
(use-package try)

;; Youdao Dictionay
(use-package youdao-dictionary
  :bind (("C-c Y" . youdao-dictionary-search-at-point)
         ("C-c y" . youdao-dictionary-search-at-point-tooltip))
  :config
  (evil-set-initial-state 'youdao-dictionary-mode 'emacs)
  ;; Cache documents
  (setq url-automatic-caching t)
  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t))

;; epub reader
(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Noto Serif CJK SC"
                             :height 1.0))
  (add-hook 'nov-mode-hook 'my-nov-font-setup))

(use-package eyebrowse
  :hook (after-init . eyebrowse-mode))

(use-package auto-save
  :quelpa
  (auto-save :repo "manateelazycat/auto-save" :fetcher github)
  :preface
  (defun my-auto-save-hook ()
    (require 'auto-save)
    (auto-save-enable))
  :hook (after-init . my-auto-save-hook)
  :config
  (setq auto-save-silent t)             ; quietly save
  ;; (setq auto-save-delete-trailing-whitespace t) ; automatically delete spaces at the end of the line when saving
  (setq auto-save-disable-predicates
        '((lambda ()
            (string-suffix-p
             "gpg"
             (file-name-extension (buffer-name)) t)))))

;; (setq auto-save-all-buffers nil)
;; (setq auto-save-directories '("~/Documents/notebooks")))

(use-package literate-calc-mode)

(use-package anki-editor)

;; --------------------------------------------------------------
;;                           Backup
;; --------------------------------------------------------------

;; (use-package smartparens
;; :diminish smartparens-mode
;; :hook
;; ((prog-mode text-mode outline-mode) . smartparens-mode)
;; :config
;; do not add another single quote under emacs-lisp-mode
;; (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
;; auto indent inside curly braces, and make the right brace be down below
;; (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
;; Solve the confliction between smartparens and hungry delete
;; Specifically, delete pair doesn't work when hungry delete is on.
;; The solution is found here:
;; https://github.com/syl20bnr/spacemacs/issues/6584
;; (defadvice hungry-delete-backward (before sp-delete-pair-advice activate)
;; (save-match-data (sp-delete-pair (ad-get-arg 0))))
;; )

(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
