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
  (vertico-resize nil)
  (vertico-cycle t)
  :bind
  (;; Reopen the last minibuffer session.  Needs `vertico-repeat-save' below.
   ("C-c v" . vertico-repeat)
   :map vertico-map
   ("<tab>" . vertico-insert)  ; Insert selected candidate into text area
   ("<escape>" . minibuffer-keyboard-quit) ; Close minibuffer
   ;; Delete a whole directory name at once.
   ("DEL" . vertico-directory-delete-char)
   ;; Cycle through candidate groups
   ("C-M-n" . vertico-next-group)
   ("C-M-p" . vertico-previous-group))
  :config
  ;; Clean up shadowed paths, such as `~/foo/bar///' to `/'.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package crm
  :ensure nil
  :demand t
  :custom
  (crm-prompt "[CRM] %p"))

(use-package emacs
  :ensure nil
  :init
  ;; For the `*Completions*' buffer when Vertico isn't in use.
  (setopt completions-format 'one-column)

  ;; Do not allow the cursor in the minibuffer prompt
  (setopt minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Enable recursive minibuffers
  (setopt enable-recursive-minibuffers t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  ;; The defaults put other styles before `completion-styles' for buffers,
  ;; project files, xref, etc.  Clear them so that orderless comes first.
  ;; Packages loaded later, such as eglot, can still add their own.
  (completion-category-defaults nil)
  ;; `partial-completion' expands `/u/s/l' to `/usr/share/lib'.
  (completion-category-overrides '((file (styles orderless partial-completion)))))

(use-package marginalia
  :bind ("M-A" . marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package consult
  :custom
  (consult-narrow-key "<")
  ;; Make async sources such as ripgrep respond faster.
  (consult-async-min-input 2)         ; default 3
  (consult-async-input-throttle 0.2)  ; default 0.5
  (consult-async-input-debounce 0.1)  ; default 0.2
  (consult-async-refresh-delay 0.15)  ; default 0.2
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
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
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
  (setopt register-preview-delay 0.5
          xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  :config
  ;; Delay the preview for expensive commands.
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

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  ;; Preview while moving around in an embark collect buffer.
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package nerd-icons-completion
  :after vertico
  :hook vertico-mode
  ;; In `:preface', so that the byte-compiler knows its functions when it
  ;; compiles the method below.
  :preface (require 'kind-nerd-icons)
  :config
  (cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql imenu)))
    "Return the icon for the `consult-imenu' candidate CAND.
A language server gives the kind in the `imenu-kind' property.  Otherwise
use the heading, as in \"HEADING name\"."
    (let* ((pos (text-property-not-all 0 (length cand) 'imenu-kind nil cand))
           (kind (if pos
                     (intern (downcase (get-text-property pos 'imenu-kind cand)))
                   (kind-nerd-icons-from-label (car (split-string cand " ")))))
           (icon (kind-nerd-icons-icon kind)))
      (if icon (concat icon " ") ""))))

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

;; Icons for breadcrumb, the same as in corfu.
(use-package breadcrumb-nerd-icons
  :load-path yilin/site-lisp-directory
  :after breadcrumb
  :demand t
  :config
  (breadcrumb-nerd-icons-mode 1))

(provide 'init-search)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-search.el ends here
