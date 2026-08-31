;;; init-search.el --- Minibuffer, search, and navigation UI. -*- lexical-binding: t -*-

;;; Commentary:
;; Configure minibuffer completion, search commands, and navigation aids.

;;; Code:

(use-package which-key
  :ensure nil
  :hook after-init)

(use-package savehist
  :ensure nil
  :init (savehist-mode 1))

(use-package vertico
  :hook after-init
  :custom
  (vertico-count 10)                    ; Number of candidates to display
  (vertico-resize nil)
  (vertico-cycle t) ; Go from last to first candidate and first to last (cycle)?
  :bind
  (;; Re-run a past minibuffer session (needs `vertico-repeat-save' below).
   ("C-c v" . vertico-repeat)
   :map vertico-map
   ("<tab>" . vertico-insert)  ; Insert selected candidate into text area
   ("<escape>" . minibuffer-keyboard-quit) ; Close minibuffer
   ;; Delete a whole directory component at once when completing file names.
   ("DEL" . vertico-directory-delete-char)
   ;; Cycle through candidate groups
   ("C-M-n" . vertico-next-group)
   ("C-M-p" . vertico-previous-group))
  :config
  ;; Tidy up shadowed file paths as you move between directories:
  ;; `~/foo/bar///' collapses to `/', and `~/foo/bar/~/' to `~/'.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  ;; Record each session so `vertico-repeat' has something to resume.
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package emacs
  :ensure nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setopt minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Enable recursive minibuffers
  (setopt enable-recursive-minibuffers t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  ;; Emacs ships per-category defaults (buffer, project-file, xref-location,
  ;; info-menu, ...) that pin those categories to `basic'/`substring', and
  ;; they take precedence over `completion-styles' -- so orderless would
  ;; silently not apply to `C-x b', project file lookup, xref and friends.
  ;; Clear them so orderless really is the completion style everywhere.
  (completion-category-defaults nil)
  ;; Keep `partial-completion' for its `/u/s/l' -> `/usr/share/lib' path
  ;; expansion, but let orderless match file names too.
  (completion-category-overrides '((file (styles orderless partial-completion)))))

(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; Deferred via :hook. The marginalia README puts `(marginalia-mode)'
  ;; in :init which forces eager loading -- we defer it instead so
  ;; `use-package-always-defer' is honored.
  :hook (after-init . marginalia-mode))

(use-package consult
  :custom
  (consult-narrow-key "<")
  ;; Start searching one character sooner, and cut the throttling roughly in
  ;; half, so async sources (ripgrep, find, ...) keep up with typing instead
  ;; of lagging a beat behind.
  (consult-async-min-input 2)         ; default 3
  (consult-async-input-throttle 0.2)  ; default 0.5
  (consult-async-input-debounce 0.1)  ; default 0.2
  (consult-async-refresh-delay 0.15)  ; default 0.2
  ;; Report line numbers relative to the whole buffer, not the current
  ;; narrowing.
  (consult-line-numbers-widen t)
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (("C-s" . consult-line)
         ("C-c i" . consult-minor-mode-menu)
         ;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-Y" . consult-yank-pop)                ;; orig. yank-pop (M-y is for yasnippet, use Y instead)
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; These core options must take effect before Consult itself is loaded.
  (setopt register-preview-delay 0.5
          xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)
  ;; This is runtime plumbing rather than a user option.
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  :config
  ;; Debounce preview for the more expensive commands.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; This command variable is not a Customize user option.
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package nerd-icons-completion
  :after vertico
  :hook vertico-mode)

(use-package color-rg
  :vc (:url "https://github.com/manateelazycat/color-rg.git")
  :after meow
  :hook (color-rg-mode . yilin/disable-meow)
  :commands (color-rg-search-input
             color-rg-search-symbol
             color-rg-search-input-in-project
             color-rg-search-symbol-in-project
             color-rg-search-symbol-in-current-file
             color-rg-search-input-in-current-file
             color-rg-search-project-rails
             color-rg-search-symbol-with-type
             color-rg-search-project-with-type
             color-rg-search-project-rails-with-type))

(use-package breadcrumb
  :hook ((prog-mode org-mode) . breadcrumb-local-mode))

;; A custom package: file and symbol-kind icons in the breadcrumb header
;; line, sharing corfu's icon table so a symbol looks the same in both.
(use-package breadcrumb-nerd-icons
  :ensure nil
  :load-path yilin/site-lisp-directory
  :after breadcrumb
  :demand t
  :config
  (breadcrumb-nerd-icons-mode 1))

(provide 'init-search)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-search.el ends here
