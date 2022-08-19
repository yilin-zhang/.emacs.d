;; init-prog.el --- Configurations for programming languages. -*- lexical-binding: t -*-

;; outline-minor-mode and hs-minor-mode must be enabled in order to use bicycle
(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . outline-minor-mode)
  (prog-mode . hs-minor-mode))

;; --------------------------------------------------------------
;;                     Checker Configuration
;; --------------------------------------------------------------
;; (use-package flycheck
;;   :diminish flycheck-mode
;;   :hook (after-init . global-flycheck-mode)
;;   :config
;;   (setq flycheck-indication-mode 'right-fringe)
;;   (setq flycheck-emacs-lisp-load-path 'inherit)
;;   ;; Only check while saving and opening files
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; ;; :config (add-to-list 'flycheck-posframe-inhibit-functions
;;                      #'(lambda () (bound-and-true-p company-backend)))))

;; --------------------------------------------------------------
;;                     LSP Configurations
;; --------------------------------------------------------------
;; (use-package ccls
;; :hook ((c-mode c++-mode objc-mode cuda-mode) .
;; (lambda () (require 'ccls) (lsp))))

;; (use-package lsp-mode
;;   :hook
;;   (python-mode . lsp)
;;   (ruby-mode . lsp)
;;   (c++-mode . lsp)
;;   (rust-mode . lsp)
;;   :init
;;   (setq lsp-auto-guess-root t         ; Detect project root
;;         ;; lsp-keep-workspace-alive nil ; Auto-kill LSP server
;;         ;; lsp-prefer-flymake nil
;;         ;; lsp-enable-on-type-formatting nil
;;         ;; lsp-enable-indentation nil
;;         lsp-signature-auto-activate nil
;;         lsp-enable-symbol-highlighting nil) ; disable the pop-up doc below the modeline
;;   :commands lsp
;;   :config
;;   (use-package lsp-ui
;;     :commands lsp-ui-mode
;;     :bind
;;     (("C-c u" . lsp-ui-imenu)
;;      :map lsp-ui-mode-map
;;      ("C-c C-d" . lsp-ui-peek-find-definitions)
;;      ("C-c C-f" . lsp-ui-peek-find-references)
;;      ("s-j" . lsp-ui-doc-glance))
;;     :init
;;     (setq
;;      lsp-ui-doc-enable nil
;;      lsp-ui-doc-header t
;;      lsp-ui-doc-include-signature t
;;      lsp-ui-doc-position 'top
;;      lsp-ui-doc-border (face-foreground 'default)))
;;   (use-package company-lsp
;;     :init (setq company-lsp-cache-candidates 'auto)
;;     :commands company-lsp
;;     :config
;;     ;; WORKAROUND:Fix tons of unrelated completion candidates shown
;;     ;; when a candidate is fulfilled
;;     ;; @see https://github.com/emacs-lsp/lsp-python-ms/issues/79
;;     (add-to-list 'company-lsp-filter-candidates '(mspyls)))
;;   (use-package dap-mode))

(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode) . eglot-ensure))
  :bind
  ("s-b" . xref-find-definitions)
  ("s-r" . xref-find-references)
  :init
  (setq-default eglot-workspace-configuration
                '((pyls
                   (plugins
                    (pycodestyle (enabled . nil))
                    (pyflakes (enabled . t))
                    (flake8 (enabled . nil)))))))

;; --------------------------------------------------------------
;;                     C/C++ Mode Configurations
;; --------------------------------------------------------------
(use-package cmake-mode)

;; --------------------------------------------------------------
;;                     Lisp Mode Configurations
;; --------------------------------------------------------------

(use-package elispfl
  :quelpa
  (elispfl :repo "cireu/elispfl" :fetcher github)
  :hook (emacs-lisp-mode . elispfl-mode))

;; --------------------------------------------------------------
;;                     Python Mode Configurations
;; --------------------------------------------------------------

(use-package pyvenv
  :init
  (setenv "WORKON_HOME" "~/miniconda3/envs"))

;; This package requires `lispy', which should be installed first
;; (use-package lpy
;;   :load-path "site-lisp/lpy"
;;   :ensure nil)

(use-package ein)

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
  :hook (ruby-mode . rubocop-mode))

;; --------------------------------------------------------------
;;                     JS2 Mode Configuration
;; --------------------------------------------------------------
(use-package web-mode
  :init
  (setq auto-mode-alist
        (append
         '(("\\.html\\'" . web-mode))
         auto-mode-alist)))

(use-package js2-mode
  :ensure t
  :init
  (setq auto-mode-alist
        (append
         '(("\\.js\\'" . js2-mode))
         auto-mode-alist)))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))
;; --------------------------------------------------------------
;;                     Racket Mode Configurations
;; --------------------------------------------------------------
(use-package racket-mode)

;; --------------------------------------------------------------
;;                     SML Mode Configurations
;; --------------------------------------------------------------
(use-package sml-mode)

;; --------------------------------------------------------------
;;                     Rust Mode Configurations
;; --------------------------------------------------------------
(use-package rust-mode)

;; --------------------------------------------------------------
;;                     Lua Mode Configurations
;; --------------------------------------------------------------
(use-package lua-mode)

;; --------------------------------------------------------------
;;                         Statistics
;; --------------------------------------------------------------
(use-package ess)

;; --------------------------------------------------------------
;;                         Common Lisp
;; --------------------------------------------------------------
(use-package sly)

;; --------------------------------------------------------------
;;                         Live Coding
;; --------------------------------------------------------------

;; (defvar foxdot-cli-path "~/.emacs.d/site-lisp/")
;; (use-package foxdot-mode
;; :load-path "site-lisp"
;; :init (require 'foxdot-mode)
;; :ensure nil)

;; :init
;; (setenv "PATH" (concat (getenv "PATH") (concat ":" (getenv "HOME") "/.ghcup/bin")))
;; (setenv "PATH" (concat (getenv "PATH") (concat ":" (getenv "HOME") "/.cabal/bin")))
;; (setq exec-path (append exec-path `(,(concat (getenv "HOME") "/.ghcup/bin/"))))
;; (setq exec-path (append exec-path `(,(concat (getenv "HOME") "/.cabal/bin/")))))

;; (use-package flymake
;;   :ensure t
;;   :config
;;   (setq flymake-fringe-indicator-position 'right-fringe)
;;   (use-package flymake-diagnostic-at-point
;;     :ensure t
;;     :after flymake
;;     :custom
;;     (flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup)
;;     :hook
;;     (flymake-mode . flymake-diagnostic-at-point-mode)
;;     :config
;;     (setq flymake-diagnostic-at-point-timer-delay 1)))

;; ;; Set the path for clangd
;; (if (eq system-type 'darwin)
;;     (let ((llvm-bin "/usr/local/opt/llvm/bin"))
;;       (if (file-directory-p llvm-bin)
;;           (progn
;;             (setenv "PATH" (concat llvm-bin path-separator (getenv "PATH")))
;;             (add-to-list 'exec-path llvm-bin)))))


;; optionally
;; optionally if you want to use debugger
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
