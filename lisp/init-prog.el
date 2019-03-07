;; init-prog.el --- Configurations for programming languages. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                     Global Configuration
;; --------------------------------------------------------------
(diminish 'eldoc-mode)

;; --------------------------------------------------------------
;;                     LSP Configurations
;; --------------------------------------------------------------
;; https://github.com/emacs-lsp/lsp-mode

;; (use-package lsp-mode
;; :ensure t
;; :commands lsp
;; :hook (prog-mode . lsp))

;; (use-package lsp-ui
;; :ensure t
;; :commands lsp-ui-mode)

;; (use-package company-lsp
;; :ensure t
;; :commands company-lsp)

(use-package eglot
  :ensure t
  :hook (prog-mode . eglot-ensure))

;; --------------------------------------------------------------
;;                     Python Mode Configurations
;; --------------------------------------------------------------
(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/miniconda3/envs"))

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
