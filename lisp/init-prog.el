;;; init-prog.el --- Configurations for programming languages. -*- lexical-binding: t -*-

;;; Commentary:
;; Configure programming modes, syntax checking, language servers, debugging,
;; and language-specific tooling.

;;; Code:

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  ;; The same keys flycheck uses under its C-c ! prefix.
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! L" . flymake-show-project-diagnostics))
  :custom
  ;; diff-hl owns the left fringe (see init-git.el), so give flymake the
  ;; right one, or the two indicators overlap.
  (flymake-fringe-indicator-position 'right-fringe)
  ;; Messages show through Eldoc in the echo area once point is on the
  ;; diagnostic, never as text at the end of the line.
  (flymake-show-diagnostics-at-end-of-line nil)
  ;; The elisp checker byte-compiles the buffer, which runs its macros
  ;; (CVE-2024-53920), so flymake only runs it on trusted files.
  (trusted-content '("~/.emacs.d/"))
  :config
  ;; One underline style for every level: dots instead of the theme's
  ;; waves. Errors and warnings keep the theme's level color so they still
  ;; stand out; notes, where the rustc lints demoted in the Rust section
  ;; land, use the text's own color like
  ;; `eglot-diagnostic-tag-unnecessary-face'. Themes define these faces,
  ;; so reapply after each theme is enabled.
  (defun yilin/flymake-restyle-faces (&rest _)
    (pcase-dolist (`(,face . ,source) '((flymake-error . error)
                                        (flymake-warning . warning)
                                        (flymake-note . nil)))
      (set-face-attribute face nil
                          :underline
                          (list :style 'dots
                                :color (if source
                                           (face-foreground source nil t)
                                         'foreground-color)))))
  (yilin/flymake-restyle-faces)
  (add-hook 'enable-theme-functions #'yilin/flymake-restyle-faces)

  (defun yilin/elisp-flymake-inherit-load-path (fn &rest args)
    "Call FN with ARGS, letting the elisp checker see `load-path'.
Without this, every `require' of an installed package is reported as
a missing file."
    (let ((elisp-flymake-byte-compile-load-path
           (append elisp-flymake-byte-compile-load-path load-path)))
      (apply fn args)))
  (advice-add 'elisp-flymake-byte-compile :around
              #'yilin/elisp-flymake-inherit-load-path)

  ;; Keep the diagnostics list a quarter-height window below the current
  ;; one, and don't let it steal point.
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
;; Bump tree-sitter fontification from the default of 3 to 4 (the max).
;; At level 3 operators, delimiters, function calls, property accesses,
;; `self'/`cls', escape sequences etc. stay the default foreground color,
;; which makes buffers look "flat" / under-highlighted compared to
;; VSCode/Zed. Level 4 colors them by category. Must be set before any
;; `*-ts-mode' fontifies a buffer.
(setopt treesit-font-lock-level 4)

;; Prefer every tree-sitter mode known to Emacs.  Missing grammars are
;; offered for installation according to `treesit-auto-install-grammar'.
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
              ;; The echo area is only a one-line glance; this opens the
              ;; full, rendered docs for the symbol at point in a buffer.
              ("C-c C-d" . yilin/eglot-documentation-at-point))
  :custom
  (eglot-events-buffer-config '(:size 0 :format full))
  ;; Nothing ignored: we want hover docs (a quick glance in the echo
  ;; area) AND documentHighlight (same-symbol highlighting) both on.
  (eglot-ignored-server-capabilities nil)
  (eglot-autoshutdown t)
  ;; Announce available code actions in the Eldoc line only. The default
  ;; also puts a marker in the left fringe, which diff-hl already uses,
  ;; and another in the margin.
  (eglot-code-action-indications '(eldoc-hint))
  ;; Keep the echo-area glance to a tidy single line. The echo area
  ;; can't render markdown (no wrapping/fontification), so multi-line
  ;; there is just a "blob" -- real docs go to the buffer below.
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
  :custom-face
  ;; Unused code (rust-analyzer's dead_code and friends) is greyed out by
  ;; default through `shadow', which hides the syntax colors and makes the
  ;; name hard to read. Keep the colors and mark it with a dotted underline
  ;; in the text's own color instead.
  (eglot-diagnostic-tag-unnecessary-face ((t (:inherit nil :underline (:style dots)))))
  :config
  (add-to-list 'eglot-server-programs
               '((json-mode json-ts-mode) . ("vscode-json-languageserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(swift-mode . ("sourcekit-lsp")))
  ;; Eglot's built-in Ruby entry offers solargraph first and falls back
  ;; to ruby-lsp; pin ruby-lsp so the choice never depends on what
  ;; happens to be installed. `add-to-list' prepends, so this wins.
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) . ("ruby-lsp")))

  ;; Documentation in a dedicated buffer, the way doom does it: the echo
  ;; area can't render LSP hover markdown, so mirror doom's
  ;; `+eglot-lookup-documentation' -- fetch hover synchronously and show
  ;; the rendered (fontified, wrapped, scrollable, copyable) contents in
  ;; a *eglot-help* window.
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
  :custom
  ;; Info buffers to the left.
  (dape-buffer-window-arrangement 'left)
  ;; Show inlay hints.
  (dape-inlay-hints t)
  :config
  ;; Save breakpoints on quit, but only after dape has actually been used.
  ;; A top-level `kill-emacs' hook would autoload dape during every shutdown.
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
  (c-ts-mode-indent-offset 4)
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
    "Return the `.venv' directory governing `default-directory', or nil.
Only a venv with a usable interpreter counts."
    (when-let* ((root (locate-dominating-file default-directory ".venv"))
                (venv (expand-file-name ".venv" root))
                ((file-executable-p (expand-file-name "bin/python" venv))))
      venv))

  (defun yilin/python-activate-venv ()
    "Point this buffer at its project\\='s `.venv', buffer-locally.

Unlike `pyvenv-activate', nothing global is mutated: the environment
lives in this buffer only, so several projects can be open at once and
each still gets its own interpreter.  Because this runs from
`python-base-mode-hook', the environment is already in place whenever
\\[eglot] is called later: eglot inherits the buffer\\='s
`process-environment' and `exec-path' when it launches the server.
The interpreter pyright resolves imports against is sent separately,
by `yilin/eglot-workspace-configuration'."
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

  ;; Plain `add-hook' rather than use-package's `:hook': the latter would
  ;; autoload `yilin/python-activate-venv' from `python', where it does
  ;; not live.
  (add-hook 'python-base-mode-hook #'yilin/python-activate-venv))

(defun yilin/generate-pyrightconfig ()
  "Generate a pyrightconfig.json file in the current directory.

When the directory holds a usable `.venv\\=', record it as venvPath/venv so
that a bare `pyright\\=' in a terminal resolves imports against the same
interpreter eglot uses; `yilin/python-activate-venv\\=' only reaches the
server eglot starts.  The pair is omitted when there is no venv, since
pyright reports a venv it cannot find."
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
  (web-mode . (lambda () (setopt-local tab-width web-mode-indent-style)))
  :custom
  (web-mode-auto-close-style 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-part-padding 0)
  (web-mode-block-padding 0)
  (web-mode-style-padding 0)
  (web-mode-script-padding 0))

(use-package rainbow-mode
  :hook ((css-mode css-ts-mode js-mode js-ts-mode typescript-ts-mode tsx-ts-mode)
         . rainbow-mode))

(use-package js-mode
  :ensure nil
  :custom (js-indent-level 2))  ; this indent level also applies to json-mode

(use-package css-mode
  :ensure nil
  :custom (css-indent-offset 2))

(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :custom
  (typescript-ts-mode-indent-offset 2))

;; --------------------------------------------------------------
;;                       Ruby Configurations
;; --------------------------------------------------------------
;; RBS is Ruby's type-signature language, and it is not a subset of Ruby --
;; `def []: (int start, ?int length) -> String?' is unparseable by
;; `ruby-mode', which mangles indentation and fontification. Signature files
;; get their own mode. Reading only: the language server for RBS itself is
;; Steep, not ruby-lsp, so nothing is registered with Eglot here.
(use-package rbs-mode
  :mode "\\.rbs\\'")

;; --------------------------------------------------------------
;;                       Rust Configurations
;; --------------------------------------------------------------
(use-package rust-mode
  :init
  ;; Build `rust-mode' on top of `rust-ts-mode', so .rs files get
  ;; tree-sitter highlighting and rust-mode's commands (C-c C-c C-u/C-k/
  ;; C-t/C-r/C-l for cargo, C-c C-d for dbg!, `rust-format-on-save').
  ;; Must be set before rust-mode loads.
  (setopt rust-mode-treesitter-derive t)
  ;; `treesit-enabled-modes' remaps `rust-mode' to `rust-ts-mode', which
  ;; would skip rust-mode again.
  (setq major-mode-remap-alist
        (delete '(rust-mode . rust-ts-mode) major-mode-remap-alist)))

;; Eglot reads this variable from its global value or from .dir-locals.el,
;; never from a buffer-local value set in a mode hook. As a function it is
;; called in a temporary buffer at the project root, which is enough to
;; find a Python project's .venv.
(defun yilin/eglot-workspace-configuration (_server)
  "Settings eglot sends to language servers."
  `(;; Code that is written but not called yet is normal mid-edit, so
    ;; turn rustc's unused-code warnings into hints. They are still
    ;; marked through the `unnecessary' tag, and `cargo build' still
    ;; reports them.
    :rust-analyzer
    (:diagnostics
     (:warningsAsHint ["dead_code" "unused_variables" "unused_imports"
                       "unused_mut" "unused_assignments"]))
    ;; Resolve imports against the project's .venv.
    :python
    (:analysis (:typeCheckingMode "standard")
     ,@(when-let* ((venv (yilin/python--venv-dir)))
         `(:pythonPath ,(expand-file-name "bin/python" venv))))))

(setopt eglot-workspace-configuration #'yilin/eglot-workspace-configuration)

;; A solid underline under mutable bindings, the way RustRover marks them.
;; rust-analyzer tags them with its own `mutable' semantic-token modifier;
;; eglot only paints modifiers listed in `eglot-semantic-token-modifiers',
;; each with the face `eglot-semantic-<name>', so add both. Only servers
;; that send `mutable' are affected, which in practice is rust-analyzer.
(with-eval-after-load 'eglot
  (defface eglot-semantic-mutable '((t :underline t))
    "Face for the `mutable' semantic-token modifier from rust-analyzer."
    :group 'eglot-semantic-faces)
  (add-to-list 'eglot-semantic-token-modifiers "mutable" t)

  ;; rust-analyzer also marks compound assignments such as `+=' as
  ;; `mutable'; keep the underline on names only. The token decoder
  ;; returns ((TYPE . MODIFIERS) . FACES).
  (defun yilin/eglot-semtok-unmark-mutable-operators (decoded)
    "Drop the mutable face from operator tokens in DECODED."
    (if (equal (caar decoded) "operator")
        (cons (car decoded) (remq 'eglot-semantic-mutable (cdr decoded)))
      decoded))
  (advice-add 'eglot--semtok-decode-token :filter-return
              #'yilin/eglot-semtok-unmark-mutable-operators))

;; --------------------------------------------------------------
;;                      Swift Configurations
;; --------------------------------------------------------------
;; `sourcekit-lsp' ships with Apple's Swift toolchain. Register it explicitly
;; because Eglot has no built-in Swift server mapping.
(use-package swift-mode
  :mode "\\.swift\\'")

;; --------------------------------------------------------------
;;                       Zig Configurations
;; --------------------------------------------------------------
;; eglot already maps `zig-mode' -> ("zls"), so installing the `zls'
;; binary (brew install zls) is all that's needed.
(use-package zig-mode)

;; --------------------------------------------------------------
;;                       Lua Configurations
;; --------------------------------------------------------------
(use-package lua-mode)

(use-package pico8-mode
  :vc (:url "https://github.com/Kaali/pico8-mode.git")
  :mode "\\.p8\\'"
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
  ;; Register pico8 icon with nerd-icons if/when it's loaded.
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
