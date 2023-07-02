;; init-prog.el --- Configurations for programming languages. -*- lexical-binding: t -*-

(use-package flymake
  :ensure nil
  :custom
  (flymake-no-changes-timeout 1))

(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . outline-minor-mode))

(use-package code-cells
  :hook
  (python-mode . code-cells-mode-maybe))

(use-package jupyter
  :hook
  (jupyter-repl-interaction-mode . code-cells-mode))

;;;###autoload
(defun yilin/jupyter-smart-eval (insert)
  (interactive "P")
  (save-excursion
    (when (and (boundp 'code-cells-mode)
               (not (region-active-p)))
      (code-cells-mark-cell))
    (jupyter-eval-line-or-region insert)
    (if (region-active-p)
        (progn
          (pulse-momentary-highlight-region (region-beginning) (region-end))
          (deactivate-mark))
      (pulse-momentary-highlight-one-line))))

(with-eval-after-load 'jupyter-repl
  (require 'pulse)
  (define-key jupyter-repl-interaction-mode-map
              (kbd "C-c C-c") #'yilin/jupyter-smart-eval))

;; --------------------------------------------------------------
;;                     Tree Sitter
;; --------------------------------------------------------------
(if (< emacs-major-version 29)
    (progn
      (use-package tree-sitter
        :init (global-tree-sitter-mode t))
      (use-package tree-sitter-langs
        :hook (tree-sitter-after-on . tree-sitter-hl-mode)))

  ;; `'https://archive.casouri.cc/note/2023/tree-sitter-in-emacs-29/index.html'
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode)
          (js-mode . js-ts-mode)
          (json-mode . json-ts-mode)))
  )

;; --------------------------------------------------------------
;;                     LSP / ctags Configurations
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
  (setq eglot-events-buffer-size 0
        eglot-ignored-server-capabilities '(:hoverProvider
                                            :documentHighlightProvider)
        eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs
               '(json-mode . ("vscode-json-languageserver" "--stdio")))

  ;; Re-opens the current buffer before reconnection
  (defun yilin/eglot-reconnect ()
    (interactive)
    (let ((filepath (buffer-file-name)))
      (kill-buffer (current-buffer))
      (find-file filepath)
      (eglot-reconnect (eglot--current-server-or-lose)))))

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
  :load-path "~/.emacs.d/site-lisp/"
  :hook (emacs-lisp-mode . elispfl-mode))

;; --------------------------------------------------------------
;;                     Python Mode Configurations
;; --------------------------------------------------------------

(use-package pyvenv
  :init
  (setenv "WORKON_HOME" "~/miniconda3/envs"))

(defun yilin/generate-pyrightconfig ()
  "Generate a pyrightconfig.json file in the current directory."
  (interactive)
  (let ((pyrightconfig-path (concat default-directory "pyrightconfig.json")))
    (if (file-exists-p pyrightconfig-path)
        (message "pyrightconfig.json already exists.")
      (progn
        ;; write string to pyrightconfig-path
        (with-temp-file pyrightconfig-path
          (insert "{
  \"reportGeneralTypeIssues\": \"warning\",
  \"reportOptionalSubscript\": \"warning\",
  \"reportOptionalMemberAccess\": \"warning\",
  \"reportOptionalCall\": \"warning\",
  \"reportOptionalIterable\": \"warning\",
  \"reportOptionalContextManager\": \"warning\",
  \"reportOptionalOperand\": \"warning\",
  \"reportOptionalVariable\": \"warning\",
  \"extraPaths\": []
}"))
        (message "pyrightconfig.json generated.")))))

;; --------------------------------------------------------------
;;                     Rust Mode Configurations
;; --------------------------------------------------------------
(use-package rust-mode)

;; --------------------------------------------------------------
;;                           Container
;; --------------------------------------------------------------
(use-package dockerfile-mode)


(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
