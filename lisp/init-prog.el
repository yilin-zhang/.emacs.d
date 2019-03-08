;; init-prog.el --- Configurations for programming languages. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                     Global Configuration
;; --------------------------------------------------------------
(diminish 'eldoc-mode)

;; --------------------------------------------------------------
;;                     LSP Configurations
;; --------------------------------------------------------------
;; https://github.com/emacs-lsp/lsp-mode

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook (prog-mode . lsp)
  :init
  ;; Detect project root
  (setq lsp-auto-guess-root t))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-enable t
	lsp-ui-doc-include-signature t
	lsp-ui-doc-position 'at-point
	;; lsp-ui-doc-use-webkit t
	lsp-ui-doc-border (face-foreground 'default)

	lsp-ui-sideline-enable nil
	lsp-ui-sideline-ignore-duplicate t)
  :config
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; (use-package eglot
;; :ensure t
;; :hook (prog-mode . eglot-ensure))

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

;; --------------------------------------------------------------
;;                     Rust Mode Configurations
;; --------------------------------------------------------------

(use-package rust-mode
  :ensure t)

;; (use-package flycheck-rust
;; :ensure t
;; :hook(flycheck-mode . flycheck-rust-setup))

(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
