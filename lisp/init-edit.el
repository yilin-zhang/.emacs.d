;; init-edit.el --- Configurations for a better editing experience. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                            Key Bindings
;; --------------------------------------------------------------
;; Use M-l as backspace
;; (global-set-key (kbd "M-l") (kbd "<backspace>"))

;; Key Modifiers
(when (eq system-type 'darwin)
  ;; Compatible with Emacs Mac port
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-function-modifier 'hyper)
  (bind-keys ([(super a)] . mark-whole-buffer)
             ([(super c)] . kill-ring-save)
             ([(super l)] . goto-line)
             ([(super q)] . save-buffers-kill-emacs)
             ([(super s)] . save-buffer)
             ([(super v)] . yank)
             ([(super w)] . kill-current-buffer)
             ([(super z)] . undo)))

(when (eq system-type 'gnu/linux)
  (use-package fcitx
    :init
    (fcitx-aggressive-setup)
    :config
    (setq fcitx-use-dbus t)))

;; --------------------------------------------------------------
;;                            Meow
;; --------------------------------------------------------------

;; indent-rigid-xxx-to-tab-stop has strange behaviour in python-mode
;; where the indentation is 8 spaces. Use python-indent-shift-xxx instead.
(defun yilin/indent-right ()
  (interactive)
  (cond ((member major-mode '(python-mode python-ts-mode))
         (python-indent-shift-right (region-beginning) (region-end)
                                    python-indent-offset))
        ((member major-mode '(json-mode json-ts-mode js-mode js-ts-mode))
         (indent-rigidly (region-beginning) (region-end) js-indent-level))
        ((member major-mode '(css-mode css-ts-mode))
         (indent-rigidly (region-beginning) (region-end) css-indent-offset))
        ((member major-mode '(lua-mode pico8-mode))
         (indent-rigidly (region-beginning) (region-end) lua-indent-level))
        (t (indent-rigidly-right-to-tab-stop (region-beginning) (region-end)))))

(defun yilin/indent-left ()
  (interactive)
  (cond ((member major-mode '(python-mode python-ts-mode))
         (python-indent-shift-left (region-beginning) (region-end)
                                   python-indent-offset))
        ((member major-mode '(json-mode json-ts-mode js-mode js-ts-mode))
         (indent-rigidly (region-beginning) (region-end) (- js-indent-level)))
        ((member major-mode '(css-mode css-ts-mode))
         (indent-rigidly (region-beginning) (region-end) (- css-indent-offset)))
        ((member major-mode '(lua-mode pico8-mode))
         (indent-rigidly (region-beginning) (region-end) (- lua-indent-level)))
        (t (indent-rigidly-left-to-tab-stop (region-beginning) (region-end)))))

(defun yilin/surround-region (char)
  "Surround the selected region's non-whitespace content with CHAR.
Supports *, =, +, / and properly pairs (, [, {."
  (interactive "cEnter character to surround with: ")
  (let* ((pairs '((?\( . ?\))
                  (?\[ . ?\])
                  (?\{ . ?\})
                  (?\< . ?\>)))
         (left-char char)
         (right-char (or (cdr (assoc char pairs)) char)))
    (if (use-region-p)
        (let ((beg (region-beginning))
              (end (region-end)))
          (save-excursion
            ;; Narrow to region to simplify handling
            (save-restriction
              (narrow-to-region beg end)
              (goto-char (point-min))
              (skip-chars-forward " \t\n")
              (let ((left-pos (point)))
                (goto-char (point-max))
                (skip-chars-backward " \t\n")
                (let ((right-pos (point)))
                  ;; Insert in reverse to preserve positions
                  (goto-char right-pos)
                  (insert right-char)
                  (goto-char left-pos)
                  (insert left-char))))))
      (message "No region selected"))))

(defun yilin/surround-region-equal ()
  (interactive)
  (yilin/surround-region ?=))

(defun yilin/surround-region-plus ()
  (interactive)
  (yilin/surround-region ?+))

(defun yilin/surround-region-asterisk ()
  (interactive)
  (yilin/surround-region ?*))

(defun yilin/surround-region-dash ()
  (interactive)
  (yilin/surround-region ?-))

(defun yilin/surround-region-paren ()
  (interactive)
  (yilin/surround-region ?\())

(defun yilin/surround-region-bracket ()
  (interactive)
  (yilin/surround-region ?\[))

(defun yilin/surround-region-curly ()
  (interactive)
  (yilin/surround-region ?\{))

(defun yilin/surround-region-angle ()
  (interactive)
  (yilin/surround-region ?\<))

(defun yilin/surround-region-quote ()
  (interactive)
  (yilin/surround-region ?\'))

(defun yilin/surround-region-dquote ()
  (interactive)
  (yilin/surround-region ?\"))

(defun yilin/surround-region-slash ()
  (interactive)
  (yilin/surround-region ?/))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (setq meow-use-clipboard t)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   ;; Surround
   '("=" . yilin/surround-region-equal)
   '("+" . yilin/surround-region-plus)
   '("*" . yilin/surround-region-asterisk)
   '("-" . yilin/surround-region-dash)
   '("(" . yilin/surround-region-paren)
   '(")" . yilin/surround-region-paren)
   '("[" . yilin/surround-region-bracket)
   '("]" . yilin/surround-region-bracket)
   '("{" . yilin/surround-region-curly)
   '("}" . yilin/surround-region-curly)
   '("<" . yilin/surround-region-angle)
   '(">" . yilin/surround-region-angle)
   '("'" . yilin/surround-region-quote)
   '("\"" . yilin/surround-region-dquote)
   '("/" . yilin/surround-region-slash)
   ;; Custom leader bindings
   '("f" . find-file)
   '("b" . consult-buffer)
   '("k" . kill-buffer)
   '("p" . consult-yank-pop)
   '("o" . other-window)
   '("d" . dired-jump)
   '("s" . outline-cycle)
   '("r" . color-rg-search-input))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("<" . yilin/indent-left)
   '(">" . yilin/indent-right)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change-save)
   '("d" . meow-kill)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-block)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-to-block)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-visit)
   '("S" . meow-goto-line)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-line)
   '("V" . rectangle-mark-mode)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))
  ;; Make sure C-[ works in GUI
  (when window-system
    (define-key input-decode-map (kbd "C-[") [control-bracketleft])
    (define-key meow-insert-state-keymap [control-bracketleft] 'meow-insert-exit)
    (define-key meow-normal-state-keymap [control-bracketleft] 'ignore)))

(use-package meow
  :demand t
  :hook
  (meow-insert-exit . corfu-quit)
  ;; Should not put meow-setup under :config,
  ;; otherwise some leader bindings will not bind
  (after-init . meow-setup)
  :config
  (meow-global-mode 1))

(defun yilin/toggle-meow-mode ()
  "Toggle meow-mode"
  (interactive)
  (if (bound-and-true-p meow-mode)
      (meow-mode -1)
    (meow-mode 1)))

(global-set-key (kbd "C-z") 'yilin/toggle-meow-mode)

;; --------------------------------------------------------------
;;                         Spell checker
;; --------------------------------------------------------------
;; NOTE:
;; Loading Jinx in Emacs 29 freezes Emacs. There is a workaround but not ideal.
;; Use flyspell instead. The issue is gone in Emacs 30.
;; `https://github.com/minad/jinx/pull/91'
(if (<= emacs-major-version 29)
    (progn
      ;; config from Centaur Emacs
      (use-package flyspell
        :ensure nil
        :diminish
        :if (executable-find "aspell")
        :hook (((text-mode outline-mode) . flyspell-mode)
               (prog-mode . flyspell-prog-mode)
               (flyspell-mode . (lambda ()
                                  (dolist (key '("C-," "C-." "C-M-i" "C-c $"))
                                    (unbind-key key flyspell-mode-map)))))
        :init (setq flyspell-issue-message-flag nil
                    ispell-program-name "aspell"
                    ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))
      ;; better UI
      (use-package flyspell-correct
        :after flyspell
        :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper))))

  (use-package jinx
    :hook (emacs-startup . global-jinx-mode)
    :bind
    ("C-;" . jinx-correct)
    ("C-M-;" . jinx-languages)))

;; --------------------------------------------------------------
;;                         Template
;; --------------------------------------------------------------

(use-package yasnippet
  :diminish (yas-minor-mode)
  :hook (after-init . yas-global-mode)
  :bind
  (:map yas-minor-mode-map
        ("M-y" . yas-expand)
        ;; Disable tab expansion
        ("TAB" . nil)
        ("<tab>" . nil))
  :config
  (use-package yasnippet-snippets))

;; --------------------------------------------------------------
;;                      Parentheses and Region
;; --------------------------------------------------------------
;; enable show-paren-mode for emacs-lisp-mode
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-style 'mixed)
  :custom-face
  (show-paren-match ((t (:background ,(face-foreground 'warning))))))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; rainbow parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; --------------------------------------------------------------
;;         Whitespace, Indentation, Delete and Folding
;; --------------------------------------------------------------

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; Visualize TAB, (HARD) SPACE, NEWLINE
;; `https://github.com/condy0919/emacs-newbie/blob/master/introduction-to-builtin-modes.md'
(use-package whitespace
  :ensure nil
  :hook (prog-mode . whitespace-mode)
  :custom
  (whitespace-line-column nil)
  (whitespace-style
   '(face                               ; visualize things below:
     empty                              ; empty lines at beginning/end of buffer
     ;; lines-tail                         ; lines go beyond `fill-column'
     space-before-tab                   ; spaces before tab
     trailing                           ; trailing blanks
     tabs                               ; tabs (show by face)
     tab-mark                           ; tabs (show by symbol)
     ))
  :config
  ;; Don't use different background for tabs.
  (face-spec-set 'whitespace-tab
                 '((t :background unspecified)))
  ;; Only use background and underline for long lines, so we can still have
  ;; syntax highlight.

  ;; For some reason use face-defface-spec as spec-type doesn't work.  My guess
  ;; is it's due to the variables with the same name as the faces in
  ;; whitespace.el.  Anyway, we have to manually set some attribute to
  ;; unspecified here.
  (face-spec-set 'whitespace-line
                 '((((background light))
                    :background "#d8d8d8" :foreground unspecified
                    :underline t :weight unspecified)
                   (t
                    :background "#404040" :foreground unspecified
                    :underline t :weight unspecified)))

  ;; Use softer visual cue for space before tabs.
  (face-spec-set 'whitespace-space-before-tab
                 '((((background light))
                    :background "#d8d8d8" :foreground "#de4da1")
                   (t
                    :inherit warning
                    :background "#404040" :foreground "#ee6aa7"))))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :hook ((lisp-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1))))))

;; Note that this package has conflict with smartparens.
;; The solution is written inside smartparens' configurations.
;; (use-package hungry-delete
;;   :diminish
;;   :hook (after-init . global-hungry-delete-mode)
;;   :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; --------------------------------------------------------------
;;                            Search
;; --------------------------------------------------------------

(use-package which-key
  :diminish (which-key-mode)
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
  (:map vertico-map
        ("<tab>" . vertico-insert)  ; Insert selected candidate into text area
        ("<escape>" . minibuffer-keyboard-quit) ; Close minibuffer
        ;; Cycle through candidate groups
        ("C-M-n" . vertico-next-group)
        ("C-M-p" . vertico-previous-group)))

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
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
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
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
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

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
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
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package nerd-icons-completion
  :after vertico
  :hook vertico-mode)

(use-package color-rg
  :ensure nil
  :quelpa (color-rg :fetcher github
                    :repo "manateelazycat/color-rg")
  :hook (color-rg-mode . (lambda () (meow-mode -1)))
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
  :ensure nil
  :quelpa (breadcrumb :fetcher github
                      :repo "joaotavora/breadcrumb")
  :hook (prog-mode . breadcrumb-local-mode))

;; Keybinding reference:
;; `https://github.com/kickingvegas/casual/tree/main/docs'
(use-package casual
  :bind
  ("C-o" . casual-editkit-main-tmenu)
  (:map org-agenda-mode-map
        ("C-o" . casual-agenda-tmenu))
  (:map bookmark-bmenu-mode-map
        ("C-o" . casual-bookmarks-tmenu))
  (:map calc-mode-map
        ("C-o" . casual-calc-tmenu))
  (:map calc-alg-map
        ("C-o" . casual-calc-tmenu))
  (:map calendar-mode-map
        ("C-o" . casual-calendar-tmenu))
  (:map dired-mode-map
        ("C-o" . casual-dired-tmenu)
        ("s" . casual-dired-sort-by-tmenu)
        ("/" . casual-dired-search-replace-tmenu)
        )
  (:map ibuffer-mode-map
        ("C-o" . casual-ibuffer-tmenu)
        ("F" . casual-ibuffer-filter-tmenu)
        ("s" . casual-ibuffer-sortby-tmenu))
  (:map image-mode-map
        ("C-o" . casual-image-resize-tmenu))
  (:map Info-mode-map
        ("C-o" . casual-info-tmenu))
  (:map isearch-mode-map
        ("C-o" . casual-isearch-tmenu))
  (:map makefile-mode-map
        ("C-o" . casual-make-tmenu))
  (:map reb-mode-map
        ("C-o" . casual-re-builder-tmenu))
  (:map reb-lisp-mode-map
        ("C-o" . casual-re-builder-tmenu)))

;; --------------------------------------------------------------
;;                           Completion
;; --------------------------------------------------------------

;; Path auto completion
(use-package comint
  :ensure nil
  :init
  (add-to-list 'completion-at-point-functions #'comint--complete-file-name-data)
  (setq comint-completion-addsuffix nil
        comint-completion-autolist nil))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                   ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                    ;; Enable auto completion
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode)
  )

;; A custom package to show icons in corfu completion
;; Adapted from `'https://emacs-china.org/t/corfu-all-the-icons-icon/20907'
(use-package kind-nerd-icons
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/"
  :after (corfu nerd-icons)
  :demand t
  :config
  (add-to-list 'corfu-margin-formatters
               #'kind-nerd-icons-margin-formatter))

;; --------------------------------------------------------------
;;                            Highlight
;; --------------------------------------------------------------
;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :custom-face (hl-todo ((t (:box t :inherit))))
  :bind (:map hl-todo-mode-map
              ([C-f3] . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode)
  :config
  (setf hl-todo-keyword-faces (assoc-delete-all "XXXX*" hl-todo-keyword-faces))
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :preface
  (defun yilin/diff-hl-dired-mode-unless-remote ()
    (unless (file-remote-p default-directory)
      (diff-hl-dired-mode)))
  :custom-face
  (diff-hl-change ((t (:inherit diff-changed :foreground unspecified :background unspecified))))
  (diff-hl-insert ((t (:inherit diff-added :background unspecified))))
  (diff-hl-delete ((t (:inherit diff-removed :background unspecified))))
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . yilin/diff-hl-dired-mode-unless-remote))
  :init (setq diff-hl-draw-borders nil)
  :config
  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (with-no-warnings
    (defun my-diff-hl-fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        (vector (if (eq system-type 'gnu/linux) #b11111100 #b11100000))
        1 8
        '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

    (unless (display-graphic-p)
      ;; Fall back to the display margin since the fringe is unavailable in tty
      (diff-hl-margin-mode 1)
      ;; Avoid restoring `diff-hl-margin-mode'
      (with-eval-after-load 'desktop
        (add-to-list 'desktop-minor-mode-table
                     '(diff-hl-margin-mode nil))))

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

;; Make the cursor have a tail, which is easier for
;; users to locate the cursor.
(use-package pulsar
  :hook (after-init . pulsar-global-mode))

;; --------------------------------------------------------------
;;                            Undo
;; --------------------------------------------------------------
(use-package vundo)

;; --------------------------------------------------------------
;;                            Other
;; --------------------------------------------------------------
(use-package separedit
  :bind (:map prog-mode-map
              ("C-c '" . separedit))
  :hook (separedit-buffer-creation . auto-fill-mode)
  :custom
  (separedit-preserve-string-indentation t)
  (separedit-continue-fill-column t)
  )

;; --------------------------------------------------------------
;;                            Custom
;; --------------------------------------------------------------

;; **************************************************************
;; Unfill paragraph (the reversed operation to auto-fill)
;; `https://www.emacswiki.org/emacs/UnfillParagraph'
;; **************************************************************
(defun yilin/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(define-key global-map (kbd "M-Q") 'yilin/unfill-paragraph)

;; **************************************************************
;; Narrow and widen
;; `https://endlessparentheses.com/emacs-narrow-or-widen-dwim.html'
;; **************************************************************
(defun yilin/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))
;; This line replaces Emacs' entire narrowing keymap
(define-key ctl-x-map "n" #'yilin/narrow-or-widen-dwim)

;; **************************************************************
;; Press M-<backspace> to delete the entire file/directory name
;; **************************************************************
(defun yilin/delete-minibuffer-directory ()
  "Delete the directory name before the last slash in the minibuffer."
  (interactive)
  (let* ((end (point))
         (beg (save-excursion
                (when (eq ?/ (char-before))
                  (backward-char))
                (if (search-backward "/" nil t)
                    (1+ (point))
                  (backward-word)
                  (point)))))
    (delete-region beg end)))
(define-key minibuffer-local-map
            (kbd "M-<backspace>")
            'yilin/delete-minibuffer-directory)

;; **************************************************************
;; Insert time stamp
;; **************************************************************
(defun yilin/insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y%m%d%H%M%S")))

;; **************************************************************
;; Remove empty lines
;; **************************************************************
(defun yilin/remove-empty-lines ()
  "Remove all empty lines (containing only whitespace) in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (flush-lines "^[ \t]*$")))

;; **************************************************************
;; Quote lines
;; `http://xahlee.info/emacs/emacs/emacs_quote_lines.html'
;; **************************************************************
(defun yilin/quote-lines ()
  "Change current text block's lines to quoted lines with comma or other separator char.
  When there is a text selection, act on the selection, else, act on a text block separated by blank lines.

  For example,

  cat
  dog
  cow

  becomes

  \"cat\",
  \"dog\",
  \"cow\",

  or

  (cat)
  (dog)
  (cow)

  If the delimiter is any left bracket, the end delimiter is automatically the matching bracket.

  URL `http://ergoemacs.org/emacs/emacs_quote_lines.html'
  Version 2017-01-08"
  (interactive)
  (let* (
         $p1
         $p2
         ($quoteToUse
          (read-string
           "Quote to use:" "\"" nil
           '(
             ""
             "\""
             "'"
             "("
             "{"
             "["
             )))
         ($separator
          (read-string
           "line separator:" "," nil
           '(
             ""
             ","
             ";"
             )))
         ($beginQuote $quoteToUse)
         ($endQuote
          ;; if begin quote is a bracket, set end quote to the matching one. else, same as begin quote
          (let (($syntableValue (aref (syntax-table) (string-to-char $beginQuote))))
            (if (eq (car $syntableValue ) 4) ; ; syntax table, code 4 is open paren
                (char-to-string (cdr $syntableValue))
              $quoteToUse
              ))))
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (progn
        (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "NOERROR")
        (skip-chars-backward " \t\n" )
        (setq $p2 (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (skip-chars-forward "\t ")
        (insert $beginQuote)
        (goto-char (point-max))
        (insert $endQuote)
        (goto-char (point-min))
        (while (re-search-forward "\n\\([\t ]*\\)" nil "NOERROR" )
          (replace-match
           (concat $endQuote $separator (concat "\n" (match-string 1)) $beginQuote) "FIXEDCASE" "LITERAL"))
        ;;
        ))))

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
