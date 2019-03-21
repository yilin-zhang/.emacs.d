;; init-prog.el --- Configurations for programming languages. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                     Global Configuration
;; --------------------------------------------------------------
(diminish 'eldoc-mode)

;; --------------------------------------------------------------
;;                     Flymake Configuration
;; --------------------------------------------------------------
(use-package flymake
  :ensure nil
  :config
  (use-package flymake-diagnostic-at-point
    :ensure t
    :after flymake
    :custom
    (flymake-diagnostic-at-point-error-prefix "⚠")
    (flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup)
    :hook
    (flymake-mode . flymake-diagnostic-at-point-mode)
    :config
    (setq flymake-diagnostic-at-point-timer-delay 1)))

;; --------------------------------------------------------------
;;                     LSP Configurations
;; --------------------------------------------------------------
(use-package eglot
  :ensure t
  :bind (:map eglot-mode-map
              ("C-c d" . xref-find-definitions))
  :hook (prog-mode . eglot-ensure)
  :config
  (use-package eldoc-box
    :ensure t
    :hook
    (eglot--managed-mode . eldoc-box-hover-mode)
    ))

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
;;                     Ruby Mode Configurations
;; --------------------------------------------------------------
;; https://github.com/senny/rvm.el
(use-package rvm
  :ensure nil)

;; --------------------------------------------------------------
;;                     Rust Mode Configurations
;; --------------------------------------------------------------
(use-package rust-mode
  :ensure t)

;; (use-package flycheck-rust
;; :ensure t
;; :hook(flycheck-mode . flycheck-rust-setup))

;; --------------------------------------------------------------
;;                            Backup
;; --------------------------------------------------------------

;; ;; https://github.com/emacs-lsp/lsp-mode
;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;;   :hook (prog-mode . lsp)
;;   :init
;;   ;; Detect project root
;;   (setq lsp-auto-guess-root t)

;;   :config
;;   (use-package lsp-ui
;;     :ensure t
;;     :commands lsp-ui-mode
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
;;     :commands company-lsp))

(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
