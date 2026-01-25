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

;; --------------------------------------------------------------
;;                         Tree Sitter
;; --------------------------------------------------------------
;; `https://archive.casouri.cc/note/2023/tree-sitter-in-emacs-29/index.html'
;; (setq major-mode-remap-alist
;;       '((python-mode . python-ts-mode)
;;         (js-mode . js-ts-mode)
;;         (json-mode . json-ts-mode)
;;         (css-mode . css-ts-mode)))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  :hook (after-init . global-treesit-auto-mode))

;; --------------------------------------------------------------
;;                             LSP
;; --------------------------------------------------------------

(use-package emacs
  :ensure nil
  :bind
  (("s-b" . xref-find-definitions)
   ("s-r" . xref-find-references)))

(use-package eglot
  :preface
  ;; `https://github.com/joaotavora/eglot/discussions/1184'
  (defun yilin/vue-eglot-init-options ()
    (let ((tsdk-path
           (expand-file-name
            "lib"
            (shell-command-to-string "npm list --global --parseable typescript | head -n1 | tr -d \"\n\""))))
      `(:typescript
        (:tsdk
         ,tsdk-path
         :languageFeatures (:completion
                            (
                             :defaultTagNameCase "both"
                             :defaultAttrNameCase "kebabCase"
                             :getDocumentNameCasesRequest nil
                             :getDocumentSelectionRequest nil
                             )
                            :diagnostics
                            (:getDocumentVersionRequest nil))
         :documentFeatures (:documentFormatting
                            (
                             :defaultPrintWidth 100
                             :getDocumentPrintWidthRequest nil
                             )
                            :documentSymbol t
                            :documentColor t)))))

  ;; Re-opens the current buffer before reconnection
  (defun yilin/eglot-reconnect ()
    (interactive)
    (let ((filepath (buffer-file-name)))
      (kill-buffer (current-buffer))
      (find-file filepath)
      (eglot-reconnect (eglot--current-server-or-lose))))
  :config
  (setq eglot-events-buffer-size 0
        eglot-ignored-server-capabilities '(:hoverProvider
                                            :documentHighlightProvider)
        eglot-autoshutdown t)
  (add-to-list 'eglot-server-programs
               '(json-mode . ("vscode-json-languageserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               `(vue-mode . ("vue-language-server" "--stdio"
                             :initializationOptions ,(yilin/vue-eglot-init-options))))
  )

;; `https://github.com/jdtsmith/eglot-booster'
(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster.git")
  :if (executable-find "emacs-lsp-booster")
  :after eglot
  :config (eglot-booster-mode))

;; Config reference: `https://github.com/svaante/dape?tab=readme-ov-file#configuration'
(use-package dape
  :after meow
  :hook
  (dape-repl-mode . (lambda () (meow-mode -1)))
  ;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load)
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
(use-package elispfl
  :load-path yilin/site-lisp-directory
  :hook (emacs-lisp-mode . elispfl-mode))

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
   ("\\.djhtml\\'" . web-mode)
   ("\\.vue\\'" . vue-mode))
  :preface
  (define-derived-mode vue-mode web-mode "Vue")
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
  :hook (css-mode js-mode))

(use-package js-mode
  :ensure nil
  :custom (js-indent-level 2))  ; this indent level also applies to json-mode

(use-package css-mode
  :ensure nil
  :custom (css-indent-offset 2))

;; --------------------------------------------------------------
;;                       Rust Configurations
;; --------------------------------------------------------------
(use-package rust-mode)

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
                      :weight 'bold))

(use-package nerd-icons
  :after pico8-mode
  :config
  (add-to-list 'nerd-icons-extension-icon-alist
               '("p8" nerd-icons-sucicon "nf-seti-lua" :face nerd-icons-lpink))
  (add-to-list 'nerd-icons-mode-icon-alist
               '(pico8-mode nerd-icons-sucicon "nf-seti-lua" :face nerd-icons-lpink)))

;; --------------------------------------------------------------
;;                           Container
;; --------------------------------------------------------------
(use-package dockerfile-mode)

;; --------------------------------------------------------------
;;                            Copilot
;; --------------------------------------------------------------
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el.git")
  :bind
  (:map copilot-completion-map
        ("C-M-<return>" . copilot-accept-completion)))

(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
