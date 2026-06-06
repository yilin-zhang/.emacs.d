;; init-prog.el --- Configurations for programming languages. -*- lexical-binding: t -*-

(use-package flymake
  :ensure nil
  :custom
  (flymake-no-changes-timeout 1))

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
(setq treesit-font-lock-level 4)

(setq major-mode-remap-alist
      '(;; markup lang
        (yaml-mode . yaml-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        ;; programming
        (bash-mode . bash-ts-mode)
        (python-mode . python-ts-mode)
        (js-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (rust-mode . rust-ts-mode)))

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
  :config
  (setq eglot-events-buffer-config '(:size 0 :format full)
        ;; Nothing ignored: we want hover docs (a quick glance in the echo
        ;; area) AND documentHighlight (same-symbol highlighting) both on.
        eglot-ignored-server-capabilities nil
        eglot-autoshutdown t
        ;; Keep the echo-area glance to a tidy single line. The echo area
        ;; can't render markdown (no wrapping/fontification), so multi-line
        ;; there is just a "blob" -- real docs go to the buffer below.
        eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
  (add-to-list 'eglot-server-programs
               '((json-mode json-ts-mode) . ("vscode-json-languageserver" "--stdio")))

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

;; `https://github.com/jdtsmith/eglot-booster'
(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster.git")
  :if (executable-find "emacs-lsp-booster")
  :after eglot
  :config (eglot-booster-mode))

;; Config reference: `https://github.com/svaante/dape?tab=readme-ov-file#configuration'
(use-package dape
  :hook
  (dape-repl-mode . yilin/disable-meow)
  ;; Save breakpoints on quit. (Loading breakpoints used to live on
  ;; `after-init', which forced dape to load at startup whether or
  ;; not you ever debug. Removed -- run `M-x dape-breakpoint-load'
  ;; manually if you want last session's breakpoints back.)
  (kill-emacs . dape-breakpoint-save)
  :config
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)
  ;; Info buffers to the left
  (setq dape-buffer-window-arrangement 'left)
  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)
  ;; Showing inlay hints
  (setq dape-inlay-hints t)
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
(use-package pyvenv
  :init
  (defun yilin/pyvenv-activate-project (&optional path)
    "Activate Python virtual environment for a project.
With prefix arg, prompt for a directory and activate its project's venv.
Otherwise, activate venv for the current project.

Assumes a '.venv' directory exists at the project root.
Requires `project-current' to identify the project."
    (interactive (list (when current-prefix-arg
                         (read-directory-name "Project path: "))))
    (let* ((dir (or path default-directory))
           (proj (project-current nil dir))
           (root (when proj (project-root proj)))
           (venv-dir (when root (expand-file-name ".venv" root))))
      (cond
       ((not proj)
        (user-error "No project found for %s" dir))
       ((not (file-directory-p venv-dir))
        (user-error "No .venv directory at %s" venv-dir))
       (t
        (pyvenv-activate venv-dir)
        (message "%s activated" venv-dir))))))

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
  \"reportArgumentType\": \"warning\",
  \"extraPaths\": []
}"))
        (message "pyrightconfig.json generated.")))))

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
  (web-mode . (lambda () (setq-local tab-width web-mode-indent-style)))
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
;;                       Rust Configurations
;; --------------------------------------------------------------
(use-package rust-mode)

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
  (pico8-mode . (lambda () (setq-local lua-indent-level 1)))
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
