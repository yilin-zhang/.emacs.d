;; init-prog.el --- Configurations for programming languages. -*- lexical-binding: t -*-

(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . outline-minor-mode))

;; --------------------------------------------------------------
;;                     Tree Sitter
;; --------------------------------------------------------------
(use-package tree-sitter
  :init (global-tree-sitter-mode t))

(use-package tree-sitter-langs
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

;; --------------------------------------------------------------
;;                     LSP Configurations
;; --------------------------------------------------------------
(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode
                                                'lisp-mode
                                                'makefile-mode
                                                'web-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode) . eglot-ensure))
  :bind
  ("s-b" . xref-find-definitions)
  ("s-r" . xref-find-references)
  :config
  (setq eglot-events-buffer-size 0)
  (add-to-list 'eglot-server-programs
               '(json-mode . ("vscode-json-languageserver" "--stdio"))))

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

;; --------------------------------------------------------------
;;                     Web Configuration
;; --------------------------------------------------------------
(use-package web-mode
  :init
  (setq auto-mode-alist
        (append
         '(("\\.html\\'" . web-mode))
         auto-mode-alist)))

;; set indentation for json-mode
;; make it a local variable so that it doesn't conflict with the indentation in js mode
(use-package json-mode
  :ensure nil
  :hook (json-mode . (lambda ()
                       (make-local-variable 'js-indent-level)
                       (setq js-indent-level 2))))


(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
