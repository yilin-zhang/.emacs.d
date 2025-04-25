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
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)))
  )

;; --------------------------------------------------------------
;;                             LSP
;; --------------------------------------------------------------

(global-set-key (kbd "s-b") 'xref-find-definitions)
(global-set-key (kbd "s-r") 'xref-find-references)

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

(use-package eglot
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
;;                        C/C++ Configurations
;; --------------------------------------------------------------
(use-package cmake-mode)

;; --------------------------------------------------------------
;;                        Lisp Configurations
;; --------------------------------------------------------------
(use-package elispfl
  :load-path "~/.emacs.d/site-lisp/"
  :hook (emacs-lisp-mode . elispfl-mode))

;; --------------------------------------------------------------
;;                       Python Configurations
;; --------------------------------------------------------------

(use-package pyvenv)

(use-package jupyter
  :after code-cells
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
              (kbd "C-<return>") #'yilin/jupyter-smart-eval))

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
;;                      Web Configuration
;; --------------------------------------------------------------
(use-package web-mode
  :mode
  ("\\.html\\'"
   "\\.phtml\\'"
   "\\.tpl\\.php\\'"
   "\\.[agj]sp\\'"
   "\\.as[cp]x\\'"
   "\\.erb\\'"
   "\\.mustache\\'"
   "\\.djhtml\\'")
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

(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

;; --------------------------------------------------------------
;;                       Rust Configurations
;; --------------------------------------------------------------
(use-package rust-mode)

;; --------------------------------------------------------------
;;                       Lua Configurations
;; --------------------------------------------------------------
(use-package lua-mode)

;;;###autoload
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

(use-package pico8-mode
  :ensure nil
  :quelpa (pico8-mode :fetcher github
                      :repo "Kaali/pico8-mode"
                      :branch "master"
                      :files ("dist" "*.el"))
  :mode "\\.p8\\'"
  :hook
  (pico8-mode . (lambda () (setq-local lua-indent-level 1)))
  (pico8-mode . yilin/pico8-narrow-buffer)
  :config
  (set-face-attribute 'pico8--non-lua-overlay nil
                      :foreground (face-foreground 'shadow)
                      :weight 'bold))

(with-eval-after-load 'nerd-icons
  (add-to-list 'nerd-icons-extension-icon-alist
               '("p8" nerd-icons-sucicon "nf-seti-lua" :face nerd-icons-lpink))
  (add-to-list 'nerd-icons-mode-icon-alist
               '(pico8-mode nerd-icons-sucicon "nf-seti-lua" :face nerd-icons-lpink))
  )

;; --------------------------------------------------------------
;;                           Container
;; --------------------------------------------------------------
(use-package dockerfile-mode)


(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
