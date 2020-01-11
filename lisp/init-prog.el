;; init-prog.el --- Configurations for programming languages. -*- lexical-binding: t -*-

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
  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; ;; :config (add-to-list 'flycheck-posframe-inhibit-functions
;;                      #'(lambda () (bound-and-true-p company-backend)))))
;; --------------------------------------------------------------
;;                     LSP Configurations
;; --------------------------------------------------------------
(use-package lsp-mode
  :ensure t
  :hook
  (python-mode . lsp)
  (ruby-mode . lsp)
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point))
  :init
  (setq lsp-auto-guess-root t        ; Detect project root
        lsp-keep-workspace-alive nil ; Auto-kill LSP server
        lsp-prefer-flymake nil)       ; Use lsp-ui and flycheck
  ;;flymake-fringe-indicator-position 'right-fringe)
  :commands lsp
  :config
  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode
    :init
    (setq lsp-ui-doc-enable t
          lsp-ui-doc-header t
          lsp-ui-doc-include-signature t
          lsp-ui-doc-position 'top
          ;; lsp-ui-doc-use-webkit t
          lsp-ui-doc-border (face-foreground 'default)))
  (use-package company-lsp
    :ensure t
    :commands company-lsp)
  (use-package dap-mode
    :ensure t))

;; optionally
;; optionally if you want to use debugger
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; --------------------------------------------------------------
;;                     Lisp Mode Configurations
;; --------------------------------------------------------------
(use-package lispy
  :ensure t)

;; --------------------------------------------------------------
;;                     Python Mode Configurations
;; --------------------------------------------------------------

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

;;https://github.com/senny/rbenv.el
(use-package rbenv
  :load-path "site-lisp"
  :ensure nil
  :config
  (setq rbenv-show-active-ruby-in-modeline nil)
  (global-rbenv-mode 1))

(use-package rubocop
  :ensure t
  :hook (ruby-mode . rubocop-mode))

;; --------------------------------------------------------------
;;                     Racket Mode Configurations
;; --------------------------------------------------------------
(use-package racket-mode
  :ensure t)

;; --------------------------------------------------------------
;;                     SClang Configurations
;; --------------------------------------------------------------
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/scel/el")
;; (use-package sclang
;;   :init
;;   (setenv "PATH" (concat (getenv "PATH") ":/Applications/SuperCollider.app/Contents/MacOS"))
;;   (setq exec-path (append exec-path '("/Applications/SuperCollider.app/Contents/MacOS" ))))

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

(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
