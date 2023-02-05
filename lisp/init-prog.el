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
;;                     LSP / ctags Configurations
;; --------------------------------------------------------------
;; (use-package eglot
;;   :hook ((prog-mode . (lambda ()
;;                         (unless (derived-mode-p 'emacs-lisp-mode
;;                                                 'lisp-mode
;;                                                 'makefile-mode
;;                                                 'web-mode)
;;                           (eglot-ensure))))
;;          ((markdown-mode yaml-mode) . eglot-ensure))
;;   :bind
;;   ("s-b" . xref-find-definitions)
;;   ("s-r" . xref-find-references)
;;   :config
;;   (setq eglot-events-buffer-size 0)
;;   (add-to-list 'eglot-server-programs
;;                '(json-mode . ("vscode-json-languageserver" "--stdio"))))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode
                                                'lisp-mode
                                                'makefile-mode
                                                'web-mode)
                          (lsp))))
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands lsp
  :bind
  ("s-b" . lsp-find-definition)
  ("s-r" . lsp-find-references)
  :config
  (setq lsp-modeline-diagnostics-enable nil)
  )

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

(use-package citre
  :defer t
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  ;; Bind your frequently used commands.  Alternatively, you can define them
  ;; in `citre-mode-map' so you can only use them when `citre-mode' is enabled.
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  :config
  (setq
   ;; Set these if readtags/ctags is not in your path.
   ;; citre-readtags-program "/path/to/readtags"
   ;; citre-ctags-program "/path/to/ctags"
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   ;; citre-project-root-function #'projectile-project-root
   ;; Set this if you want to always use one location to create a tags file.
   ;; citre-default-create-tags-file-location 'global-cache
   ;; See the "Create tags file" section above to know these options
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   ;; By default, when you open any file, and a tags file can be found for it,
   ;; `citre-mode' is automatically enabled.  If you only want this to work for
   ;; certain modes (like `prog-mode'), set it like this.
   citre-auto-enable-citre-mode-modes nil))

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


(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
