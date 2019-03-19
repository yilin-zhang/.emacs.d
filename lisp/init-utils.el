;; init-utils.el --- Utility configurations. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                         Better Writting
;; --------------------------------------------------------------

;; center the paragraphs
;; A known bug: when linum-mode is enabled under org-mode, the screen margin
;; can be abnormal in certain cases. It might be caused by a bug of Emacs
;; before Emacs 26.1.
;; ref: https://github.com/rnkn/olivetti
;; So just setting linum-mode as a prog-mode-hook is a reasonable solution.
;; copy from Centaur Emacs
(use-package olivetti
  :diminish visual-line-mode olivetti-mode
  :bind ("<f7>" . olivetti-mode)
  :init (setq olivetti-body-width 0.618))

;; --------------------------------------------------------------
;;                           PDF Reading
;; --------------------------------------------------------------

;; `TODO' o is not binded to pdf-outline, which is weird,
;; so I force it to be binded.
(use-package pdf-tools
  :ensure t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :hook
  (pdf-view-mode . pdf-links-minor-mode)
  (pdf-view-mode . pdf-misc-context-menu-minor-mode)
  (pdf-view-mode . pdf-outline-minor-mode)
  (pdf-view-mode . pdf-annot-minor-mode)
  (pdf-view-mode . (lambda()(line-number-mode -1)))
  :bind (:map pdf-view-mode-map
              ("C-s" . pdf-occur)
              ("o" . pdf-outline))
  :config (pdf-tools-install))

;; --------------------------------------------------------------
;;                           File Tree
;; --------------------------------------------------------------
(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "<f8>") 'neotree-toggle)
  (setq neo-smart-open t))

;; --------------------------------------------------------------
;;                           Projectile
;; --------------------------------------------------------------
(use-package projectile
  :ensure t
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
  :ensure t
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
;;                           Fancy Stuff
;; --------------------------------------------------------------

;; try packages without actually install them
(use-package try
  :ensure t)

;; check weather information
(use-package wttrin
  :ensure t
  :commands (wttrin)
  :init
  (setq wttrin-default-cities '("西安" "杭州")))

;; Youdao Dictionay
(use-package youdao-dictionary
  :ensure t
  :bind (("C-c y" . youdao-dictionary-search-at-point)
     ("C-c Y" . youdao-dictionary-search-at-point-tooltip))
  :config
  ;; Cache documents
  (setq url-automatic-caching t)
  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t))

;; --------------------------------------------------------------
;;                           Backup
;; --------------------------------------------------------------

;; (use-package smartparens
;; :ensure t
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
