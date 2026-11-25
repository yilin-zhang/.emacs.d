;;; init-prog.el --- Configurations for programming languages. -*- lexical-binding: t -*-

;;; Commentary:
;; Configure programming modes, syntax checking, language servers, debugging,
;; and language-specific tooling.

;;; Code:

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  ;; The same keys as flycheck.
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! L" . flymake-show-project-diagnostics))
  :custom
  ;; diff-hl uses the left fringe.
  (flymake-fringe-indicator-position 'right-fringe)
  ;; The elisp checker runs the buffer's macros (CVE-2024-53920), so it only
  ;; runs on trusted files.  Keep third-party code out of this list.
  (trusted-content '("~/.emacs.d/early-init.el"
                     "~/.emacs.d/lisp/"
                     "~/.emacs.d/custom/"
                     "~/.emacs.d/site-lisp/"))
  :config
  (defun yilin/elisp-flymake-inherit-load-path (fn &rest args)
    "Call FN with ARGS, letting the elisp checker see `load-path'.
Otherwise it can't find installed packages."
    (let ((elisp-flymake-byte-compile-load-path
           (append elisp-flymake-byte-compile-load-path load-path)))
      (apply fn args)))
  (advice-add 'elisp-flymake-byte-compile :around
              #'yilin/elisp-flymake-inherit-load-path)

  ;; Show the diagnostics list below the current window.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Flymake diagnostics for "
                 (display-buffer-reuse-window display-buffer-below-selected)
                 (window-height . 0.25)
                 (dedicated . t)
                 (inhibit-same-window . t))))

(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . outline-minor-mode))

;; --------------------------------------------------------------
;;                         Tree Sitter
;; --------------------------------------------------------------
;; Also highlight operators, function calls, properties, etc.  Must be set
;; before a tree-sitter mode fontifies a buffer.
(setopt treesit-font-lock-level 4)

;; Use tree-sitter modes wherever possible.  Emacs offers to install missing
;; grammars (see `treesit-auto-install-grammar').
(setopt treesit-enabled-modes t)

;; --------------------------------------------------------------
;;                             LSP
;; --------------------------------------------------------------

(use-package emacs
  :ensure nil
  :bind
  (("s-b" . xref-find-definitions)
   ("s-r" . xref-find-references)))

(use-package eglot
  :bind (:map eglot-mode-map
              ;; Full documentation in a buffer.
              ("C-c C-d" . yilin/eglot-documentation-at-point))
  :custom
  (eglot-events-buffer-config '(:size 0 :format full))
  ;; No formatting while typing.
  (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
  (eglot-autoshutdown t)
  ;; Announce code actions in the echo area only.  diff-hl uses the fringe.
  (eglot-code-action-indications '(eldoc-hint))
  :custom-face
  ;; Mark unused code with a dotted underline.  The default greys it out,
  ;; which hides the syntax colors.
  (eglot-diagnostic-tag-unnecessary-face ((t (:inherit nil :underline (:style dots)))))
  :config
  ;; Eglot has no entry for Swift.
  (add-to-list 'eglot-server-programs
               '(swift-mode . ("sourcekit-lsp")))
  ;; Eglot prefers solargraph by default.
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) . ("ruby-lsp")))

  ;; Highlight rust-analyzer's own token types and the `trait' modifier.
  ;; Eglot only handles names on these lists, each with a face of that name.
  ;; The gruvbox-jb theme sets the colors.
  (dolist (name '("selfKeyword" "lifetime" "typeAlias" "union" "derive"
                  "macroBang" "trait"))
    (custom-declare-face (intern (concat "eglot-semantic-" name)) '((t))
                         (format "Face for the `%s' LSP semantic token." name)
                         :group 'eglot-semantic-fontification))
  (setq eglot-semantic-token-types
        (append eglot-semantic-token-types
                '("selfKeyword" "lifetime" "typeAlias" "union" "derive"
                  "macroBang")))
  (setq eglot-semantic-token-modifiers
        (append eglot-semantic-token-modifiers '("trait")))

  ;; Adapted from doom's `+eglot-lookup-documentation'.
  (defvar yilin/eglot--help-buffer nil)
  (defun yilin/eglot-documentation-at-point ()
    "Show LSP documentation for the symbol at point in a help buffer."
    (interactive)
    (eglot--dbind ((Hover) contents range)
        (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                         (eglot--TextDocumentPositionParams))
      (let ((blurb (and (not (seq-empty-p contents))
                        (eglot--hover-info contents range)))
            (hint (thing-at-point 'symbol)))
        (if blurb
            (with-current-buffer
                (or (and (buffer-live-p yilin/eglot--help-buffer)
                         yilin/eglot--help-buffer)
                    (setq yilin/eglot--help-buffer
                          (generate-new-buffer "*eglot-help*")))
              (with-help-window (current-buffer)
                (rename-buffer (format "*eglot-help for %s*" hint))
                (with-current-buffer standard-output (insert blurb))
                (setq-local nobreak-char-display nil)))
          (message "No documentation for %s" (or hint "symbol at point")))))))

;; Config reference: `https://github.com/svaante/dape?tab=readme-ov-file#configuration'
(use-package dape
  :hook
  (dape-repl-mode . yilin/disable-meow)
  :config
  ;; Save breakpoints on quit.  Outside :config, this would load dape on
  ;; every quit.
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save)
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)
  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)
  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))
  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook 'kill-buffer)
  )

;; --------------------------------------------------------------
;;                        C/C++ Configurations
;; --------------------------------------------------------------
(use-package cc-mode
  :ensure nil
  :custom (c-basic-offset 4))

(use-package c-ts-mode
  :ensure nil
  :custom
  (c-ts-indent-offset 4)
  (c-ts-mode-indent-style 'k&r))

(use-package cmake-mode)

;; --------------------------------------------------------------
;;                        Lisp Configurations
;; --------------------------------------------------------------
(use-package lisp-semantic-hl
  :hook ((emacs-lisp-mode lisp-mode) . lisp-semantic-hl-mode))

(use-package emacs
  :ensure nil
  :hook (emacs-lisp-mode . (lambda ()
                             (add-to-list 'imenu-generic-expression
                                          '("Sections" "^;;; \\(.+\\)$" 1)))))

;; --------------------------------------------------------------
;;                       Python Configurations
;; --------------------------------------------------------------
(use-package python
  :ensure nil
  :init
  (defun yilin/python--venv-dir ()
    "Return the `.venv' directory with a Python for this directory, or nil."
    (when-let* ((root (locate-dominating-file default-directory ".venv"))
                (venv (expand-file-name ".venv" root))
                ((file-executable-p (expand-file-name "bin/python" venv))))
      venv))

  (defun yilin/python-activate-venv ()
    "Use the project\\='s `.venv' in this buffer.
Eglot starts its server with this environment too, but pyright gets its
interpreter from `yilin/eglot-workspace-configuration'."
    (when-let* ((venv (yilin/python--venv-dir))
                (bin (expand-file-name "bin" venv))
                (python (expand-file-name "python" bin)))
      (setq-local exec-path (cons bin exec-path))
      (setq-local process-environment
                  (append (list (concat "VIRTUAL_ENV=" venv)
                                (concat "PATH=" bin path-separator (getenv "PATH"))
                                ;; A stray PYTHONHOME breaks a venv.
                                "PYTHONHOME")
                          process-environment))
      (setq-local python-shell-interpreter python)
      (setq-local python-shell-virtualenv-root venv)))

  ;; `:hook' would autoload this function from python.el.
  (add-hook 'python-base-mode-hook #'yilin/python-activate-venv)
  ;; PyCharm's Python-only colors; see site-lisp/gruvbox-jb-theme.el.
  (autoload 'gruvbox-jb-setup-python-faces "gruvbox-jb-theme")
  (add-hook 'python-ts-mode-hook #'gruvbox-jb-setup-python-faces))

(defun yilin/generate-pyrightconfig ()
  "Generate a pyrightconfig.json file in the current directory.
If there is a `.venv', include it, so that pyright in a terminal uses it
too."
  (interactive)
  (let ((path (expand-file-name "pyrightconfig.json")))
    (if (file-exists-p path)
        (message "pyrightconfig.json already exists.")
      (let ((config
             (append
              (when (file-executable-p
                     (expand-file-name ".venv/bin/python" default-directory))
                '((venvPath . ".") (venv . ".venv")))
              '((reportGeneralTypeIssues . "warning")
                (reportOptionalSubscript . "warning")
                (reportOptionalMemberAccess . "warning")
                (reportOptionalCall . "warning")
                (reportOptionalIterable . "warning")
                (reportOptionalContextManager . "warning")
                (reportOptionalOperand . "warning")
                (reportArgumentType . "warning")
                (extraPaths . [])))))
        (with-temp-file path
          (insert (json-serialize config))
          (json-pretty-print-buffer))
        (message "pyrightconfig.json generated%s."
                 (if (assq 'venvPath config) " with .venv" ""))))))

;; --------------------------------------------------------------
;;                      Web Configuration
;; --------------------------------------------------------------
(use-package web-mode
  :mode
  (("\\.html\\'" . web-mode)
   ("\\.phtml\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode))
  :hook
  (web-mode . (lambda () (setopt-local tab-width web-mode-markup-indent-offset)))
  :custom
  (web-mode-auto-close-style 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-part-padding 0)
  (web-mode-style-padding 0)
  (web-mode-script-padding 0))

(use-package rainbow-mode
  :hook ((css-mode css-ts-mode js-mode js-ts-mode typescript-ts-mode tsx-ts-mode)
         . rainbow-mode))

(use-package js
  :ensure nil
  :custom (js-indent-level 2))

(use-package css-mode
  :ensure nil
  :custom (css-indent-offset 2))

;; --------------------------------------------------------------
;;                       Ruby Configurations
;; --------------------------------------------------------------
;; Ruby's type signatures.  Eglot isn't set up for them, because their
;; language server is Steep.
(use-package rbs-mode)

;; --------------------------------------------------------------
;;                       Rust Configurations
;; --------------------------------------------------------------
(use-package rust-mode
  :init
  ;; Derive `rust-mode' from `rust-ts-mode', to have both tree-sitter and
  ;; rust-mode's commands.  Must be set before rust-mode loads.
  (setopt rust-mode-treesitter-derive t)
  ;; `treesit-enabled-modes' remaps `rust-mode' to `rust-ts-mode'.  Undo it.
  (setq major-mode-remap-alist
        (delete '(rust-mode . rust-ts-mode) major-mode-remap-alist))
  ;; RustRover's Rust-only colors; see site-lisp/gruvbox-jb-theme.el.
  (autoload 'gruvbox-jb-setup-rust-faces "gruvbox-jb-theme")
  (add-hook 'rust-mode-hook #'gruvbox-jb-setup-rust-faces))

;; Eglot ignores buffer-local values set in mode hooks, so set the global one.
;; It calls the function in a temporary buffer at the project root.
(defun yilin/eglot-workspace-configuration (_server)
  "Settings eglot sends to language servers."
  `(;; Show rustc's unused-code warnings as hints.
    :rust-analyzer
    (:diagnostics
     (:warningsAsHint ["dead_code" "unused_variables" "unused_imports"
                       "unused_mut" "unused_assignments"]))
    :python
    (:analysis (:typeCheckingMode "standard")
     ,@(when-let* ((venv (yilin/python--venv-dir)))
         `(:pythonPath ,(expand-file-name "bin/python" venv))))))

(setopt eglot-workspace-configuration #'yilin/eglot-workspace-configuration)

;; --------------------------------------------------------------
;;                      Swift Configurations
;; --------------------------------------------------------------
(use-package swift-mode)

;; --------------------------------------------------------------
;;                       Zig Configurations
;; --------------------------------------------------------------
;; Eglot needs zls (`brew install zls').
(use-package zig-mode
  :custom (zig-format-on-save nil))

;; --------------------------------------------------------------
;;                       Lua Configurations
;; --------------------------------------------------------------
(use-package lua-mode)

(use-package pico8-mode
  :vc (:url "https://github.com/Kaali/pico8-mode.git")
  :preface
  (defun yilin/pico8-narrow-buffer ()
    (interactive)
    (cl-flet ((find-string-point (str)
                (save-excursion
                  (goto-char (point-min))
                  (search-forward str nil t))))
      (let* ((lua-point (find-string-point "__lua__"))
             (gfx-point (find-string-point "__gfx__"))
             (map-point (find-string-point "__map__"))
             (sfx-point (find-string-point "__sfx__"))
             (end-point (or gfx-point map-point sfx-point (point-max))))
        (narrow-to-region (1+ lua-point)
                          (save-excursion
                            (goto-char end-point)
                            (beginning-of-line)
                            (1- (point)))))))
  :hook
  (pico8-mode . (lambda () (setopt-local lua-indent-level 1)))
  (pico8-mode . yilin/pico8-narrow-buffer)
  :config
  (set-face-attribute 'pico8--non-lua-overlay nil
                      :foreground (face-foreground 'shadow)
                      :weight 'bold)
  (with-eval-after-load 'nerd-icons
    (add-to-list 'nerd-icons-extension-icon-alist
                 '("p8" nerd-icons-sucicon "nf-seti-lua" :face nerd-icons-lpink))
    (add-to-list 'nerd-icons-mode-icon-alist
                 '(pico8-mode nerd-icons-sucicon "nf-seti-lua" :face nerd-icons-lpink))))

;; --------------------------------------------------------------
;;                           Container
;; --------------------------------------------------------------
(use-package dockerfile-mode)

(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
