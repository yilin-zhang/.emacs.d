;; init-prog.el --- Configurations for programming languages. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                     Global Configuration
;; --------------------------------------------------------------
(diminish 'eldoc-mode)

;; --------------------------------------------------------------
;;                     Checker Configuration
;; --------------------------------------------------------------
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc ruby-rubylint))
  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (use-package flycheck-posframe
    :ensure t
    :hook (flycheck-mode . flycheck-posframe-mode)))

;; ;; :config (add-to-list 'flycheck-posframe-inhibit-functions
;;                      #'(lambda () (bound-and-true-p company-backend)))))
;; --------------------------------------------------------------
;;                     LSP Configurations
;; --------------------------------------------------------------
;; (use-package eglot
;;   :ensure t
;;   :bind (:map eglot-mode-map
;;               ("C-c r" . eglot-rename))
;;   :hook (prog-mode . eglot-ensure)
;;   :config
;;   (use-package eldoc-box
;;     :ensure t
;;     :hook
;;     (eglot--managed-mode . eldoc-box-hover-mode)
;;     ))

;; --------------------------------------------------------------
;;                     Lisp Mode Configurations
;; --------------------------------------------------------------
(use-package lispy
  :ensure t)

;; --------------------------------------------------------------
;;                     Python Mode Configurations
;; --------------------------------------------------------------
(use-package anaconda-mode
  :ensure t
  :hook (python-mode . anaconda-mode)
  :bind (:map anaconda-mode-map ("C-c C-d" . anaconda-mode-show-doc))
  :config
  (use-package company-anaconda
    :ensure t
    :config
    (eval-after-load 'company
      '(add-to-list 'company-backends '(company-anaconda :with company-capf)))))

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/miniconda3/envs"))

(use-package yapfify
  :ensure t
  :diminish yapf-mode
  :hook (python-mode . yapf-mode))

;; This package requires `lispy', which should be installed first
;; (use-package lpy
;;   :load-path "site-lisp/lpy"
;;   :ensure nil)

;; --------------------------------------------------------------
;;                     Ruby Mode Configurations
;; --------------------------------------------------------------
;; Install rubocop and ruby-lint gems.

;; First start inf-ruby, then use robe-start to launch it.
(use-package robe
  :ensure t
  :hook (ruby-mode . robe-mode)
  :config
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

;;https://github.com/senny/rbenv.el
(use-package rbenv
  :load-path "site-lisp"
  :ensure nil
  :config
  (setq rbenv-show-active-ruby-in-modeline nil)
  (global-rbenv-mode 1))

(use-package ruby-electric
  :ensure t
  :hook (ruby-mode . ruby-electric-mode))

(use-package rubocop
  :ensure t
  :hook (ruby-mode . rubocop-mode))

;; --------------------------------------------------------------
;;                     Rust Mode Configurations
;; --------------------------------------------------------------
(use-package rust-mode
  :ensure t)

;; --------------------------------------------------------------
;;                     Haskell Mode Configurations
;; --------------------------------------------------------------
(use-package haskell-mode
  :ensure t
  :hook (haskell-mode . interactive-haskell-mode))

;; --------------------------------------------------------------
;;                     Racket Mode Configurations
;; --------------------------------------------------------------
(use-package racket-mode
  :ensure t)

;; --------------------------------------------------------------
;;                            Backup
;; --------------------------------------------------------------

;; (use-package flymake
;;   :ensure nil
;;   :config
;;   (setq flymake-fringe-indicator-position 'right-fringe)
;;   (use-package flymake-diagnostic-at-point
;;     :ensure t
;;     :after flymake
;;     :custom
;;     ;; (flymake-diagnostic-at-point-error-prefix "⚠ ")
;;     (flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup)
;;     :hook
;;     (flymake-mode . flymake-diagnostic-at-point-mode)
;;     :config
;;     (setq flymake-diagnostic-at-point-timer-delay 1)))

;; ;; https://github.com/emacs-lsp/lsp-mode
;; (use-package lsp-mode
;;   :ensure t
;;   :hook (prog-mode . lsp)
;;   :init
;;   (setq lsp-auto-guess-root t)  ; Detect project root
;;   (setq lsp-prefer-flymake nil) ; Use lsp-ui and flycheck

;;   :config
;;   (use-package lsp-ui
;;     :ensure t
;;     :custom-face
;;     (lsp-ui-doc-background ((t (:background nil))))
;;     (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
;;     :bind (:map lsp-ui-mode-map
;;                 ("C-c u" . lsp-ui-imenu))
;;     :init
;;     (setq lsp-ui-doc-enable t
;;           lsp-ui-doc-header t
;;           lsp-ui-doc-include-signature t
;;           lsp-ui-doc-position 'top
;;           lsp-ui-doc-use-webkit t
;;           lsp-ui-doc-border (face-foreground 'default)

;;           lsp-ui-sideline-enable nil
;;           lsp-ui-sideline-ignore-duplicate t)
;;     :config
;;     ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
;;     ;; https://github.com/emacs-lsp/lsp-ui/issues/243
;;     (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
;;       (setq mode-line-format nil)))

;;   (use-package company-lsp
;;     :ensure t
;;     :init (setq company-lsp-cache-candidates 'auto)))

(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
