;;; init-prog.el --- Configurations for programming languages. -*- lexical-binding: t -*-

;;; Commentary:
;; Configure programming modes, syntax checking, language servers, debugging,
;; and language-specific tooling.

;;; Code:

(use-package flycheck
  :hook ((prog-mode . flycheck-mode)
         (prog-mode . flycheck-annotate-mode))
  :custom
  (flycheck-idle-change-delay 1)
  ;; Surface the error message faster once point lands on it (0.9 default).
  (flycheck-display-errors-delay 0.25)
  ;; Also check buffers we only pass through briefly, so that e.g. editing
  ;; a config file refreshes the state of several buffers at once.
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-emacs-lisp-load-path 'inherit)
  ;; Annotate both the current line and the rest in the `sideline' style:
  ;; flushed right, and truncated with an ellipsis when the code leaves too
  ;; little room, rather than wrapped. The stock `below' style on the current
  ;; line pushes the code around, which is distracting while typing; the full
  ;; text of a truncated message still reaches the echo area.
  (flycheck-annotate-current-line-style 'sideline)
  (flycheck-annotate-other-lines-style 'sideline)
  ;; diff-hl owns the left fringe (see init-git.el), so give flycheck the
  ;; right one -- otherwise the two indicators overlap.
  (flycheck-indication-mode 'right-fringe)
  :custom-face
  ;; Out of the box these inherit the error-list faces, which are the theme's
  ;; full-strength red/yellow/green -- too loud for text sitting in the margin
  ;; of every other line. The level is still legible from the right-fringe
  ;; indicator and the underline on the code itself.
  (flycheck-annotate-error ((t (:inherit shadow))))
  (flycheck-annotate-warning ((t (:inherit shadow))))
  (flycheck-annotate-info ((t (:inherit shadow))))
  :preface
  (defvar yilin/flycheck--elisp-predicate nil
    "The stock `emacs-lisp' checker predicate, before our wrapper.")

  (defun yilin/flycheck-elisp-safe-p ()
    "Non-nil if it is safe to run the `emacs-lisp' checker here.
The checker macroexpands the buffer, which executes code (CVE-2024-53920),
so restrict it to buffers inside a project -- i.e. code we presumably
trust -- and honor `no-byte-compile'."
    (and (not (bound-and-true-p no-byte-compile))
         (project-current nil)
         (or (null yilin/flycheck--elisp-predicate)
             (funcall yilin/flycheck--elisp-predicate))))
  :config
  (global-flycheck-eglot-mode 1)
  ;; Rerunning the checker on every newline is excessive; `idle-change'
  ;; and `save' already cover it.
  (delq 'new-line flycheck-check-syntax-automatically)

  ;; The stock double-arrow is chunkier than it needs to be.
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center)

  ;; Wrap rather than replace the stock predicate, and only once, so
  ;; reloading this file doesn't nest the wrapper. The `eval' guards
  ;; against `setf' expanding before flycheck's gv setter exists, which
  ;; matters if this file is ever byte-compiled.
  (eval '(unless (eq (flycheck-checker-get 'emacs-lisp 'predicate)
                     #'yilin/flycheck-elisp-safe-p)
           (setq yilin/flycheck--elisp-predicate
                 (flycheck-checker-get 'emacs-lisp 'predicate))
           (setf (flycheck-checker-get 'emacs-lisp 'predicate)
                 #'yilin/flycheck-elisp-safe-p))
        t)

  ;; Keep the error list a quarter-height window below the current one,
  ;; and don't let it steal point.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Flycheck error\\(?:s\\| messages\\)\\*\\'"
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
  ;; Keep the echo-area glance to a tidy single line. The echo area
  ;; can't render markdown (no wrapping/fontification), so multi-line
  ;; there is just a "blob" -- real docs go to the buffer below.
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
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
`process-environment' and `exec-path' when it launches the server, and
`eglot-workspace-configuration' tells pyright which interpreter to
resolve imports against."
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
      (setq-local python-shell-virtualenv-root venv)
      (setq-local eglot-workspace-configuration
                  `(:python (:pythonPath ,python
                             :venvPath ,(directory-file-name
                                         (file-name-directory
                                          (directory-file-name venv)))
                             :venv ".venv")
                    :basedpyright (:analysis (:diagnosticMode "openFilesOnly"))))))

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
(use-package rust-mode)

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
