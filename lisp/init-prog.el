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
(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package lsp-mode
  :ensure t
  :hook
  (python-mode . lsp)
  (ruby-mode . lsp)
  :init
  (setq lsp-auto-guess-root t        ; Detect project root
        lsp-keep-workspace-alive nil ; Auto-kill LSP server
        lsp-prefer-flymake nil
        lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil
        lsp-enable-symbol-highlighting nil) ; disable the pop-up doc below the modeline
  :commands lsp
  :config
  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode
    :bind
    (("C-c u" . lsp-ui-imenu)
     :map lsp-ui-mode-map
     ("C-c C-d" . lsp-ui-peek-find-definitions)
     ("C-c C-f" . lsp-ui-peek-find-references)
     ("s-j" . lsp-ui-doc-glance))
    :init
    (setq
     lsp-ui-doc-enable nil
     lsp-ui-doc-header t
     lsp-ui-doc-include-signature t
     lsp-ui-doc-position 'top
     lsp-ui-doc-border (face-foreground 'default)))
  (use-package company-lsp
    :ensure t
    :init (setq company-lsp-cache-candidates 'auto)
    :commands company-lsp
    :config
    ;; WORKAROUND:Fix tons of unrelated completion candidates shown
    ;; when a candidate is fulfilled
    ;; @see https://github.com/emacs-lsp/lsp-python-ms/issues/79
    (add-to-list 'company-lsp-filter-candidates '(mspyls)))
  (use-package dap-mode
    :ensure t))
;; --------------------------------------------------------------
;;                     C/C++ Mode Configurations
;; --------------------------------------------------------------
(use-package cmake-mode
  :ensure t)

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
;;                     JS2 Mode Configuration
;; --------------------------------------------------------------
(use-package web-mode
  :ensure t
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

;; --------------------------------------------------------------
;;                     Racket Mode Configurations
;; --------------------------------------------------------------
(use-package racket-mode
  :ensure t)

;; --------------------------------------------------------------
;;                     Rust Mode Configurations
;; --------------------------------------------------------------
(use-package rust-mode
  :ensure t)

;; --------------------------------------------------------------
;;                     SClang Configurations
;; --------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/site-lisp/scel/el")
(use-package sclang
  :init
  (if (eq system-type 'darwin)
      (progn
        (setenv "PATH" (concat (getenv "PATH") ":/Applications/SuperCollider.app/Contents/MacOS"))
        (setq exec-path (append exec-path '("/Applications/SuperCollider.app/Contents/MacOS" )))))
  :config
  ;; WORKAROUND add all the prog-mode hooks to sclang-mode
  (dolist (hook prog-mode-hook)
    (add-hook 'sclang-mode-hook hook)))

;; --------------------------------------------------------------
;;                         Common Lisp
;; --------------------------------------------------------------
(use-package sly
  :ensure t)

;; --------------------------------------------------------------
;;                         Tidal
;; --------------------------------------------------------------
(use-package tidal
  :ensure t
  :init
  (setenv "PATH" (concat (getenv "PATH") (concat ":" (getenv "HOME") "/.ghcup/bin")))
  (setenv "PATH" (concat (getenv "PATH") (concat ":" (getenv "HOME") "/.cabal/bin")))
  (setq exec-path (append exec-path `(,(concat (getenv "HOME") "/.ghcup/bin/"))))
  (setq exec-path (append exec-path `(,(concat (getenv "HOME") "/.cabal/bin/")))))

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
