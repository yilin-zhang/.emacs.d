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
        mac-command-modifier 'super)
  (bind-keys ([(super a)] . mark-whole-buffer)
             ([(super c)] . kill-ring-save)
             ([(super l)] . goto-line)
             ([(super q)] . save-buffers-kill-emacs)
             ([(super s)] . save-buffer)
             ([(super v)] . yank)
             ([(super w)] . delete-frame)
             ([(super z)] . undo)))

(use-package evil
  :init
  (setq evil-want-C-i-jump nil) ;; make sure TAB is working in terminal
  (global-evil-leader-mode 1)
  (evil-mode 1)
  :config
  (use-package evil-leader
    :config
    (evil-leader/set-leader "SPC")
    (evil-leader/set-key
      "x" 'counsel-M-x
      "f" 'find-file
      "b" 'switch-to-buffer
      "k" 'kill-buffer
      "r" 'counsel-rg
      "p" 'counsel-yank-pop
      "a" 'org-agenda
      "i" 'org-clock-in
      "o" 'org-clock-out
      "g" 'magit-status
      "d" 'dired-jump))
  (use-package evil-surround
    :hook (after-init . global-evil-surround-mode)))

(when (eq system-type 'gnu/linux)
  (use-package fcitx
    :init
    (fcitx-aggressive-setup)
    :config
    (setq fcitx-use-dbus t)))

(when (eq system-type 'darwin)
  (use-package rime
    :custom
    (default-input-method "rime")
    :config
    (setq rime-librime-root "~/.emacs.d/librime/dist")
    (setq rime-show-candidate 'posframe)
    (setq rime-cursor "˰")))

;; --------------------------------------------------------------
;;                      Template and Spellchecker
;; --------------------------------------------------------------

(use-package yasnippet
  :diminish (yas-minor-mode)
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets))

;; On-the-fly spell checker
(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
         ;; WORKAROUND When using eglot, flyspell-prog-mode leads to
         ;; "<t> undefined" problem. The problem seems to be addressed
         ;; in Emacs 26.2
         ;; https://github.com/company-mode/company-mode/issues/760
         ;; (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init
  (setq flyspell-issue-message-flag nil
        ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

;; --------------------------------------------------------------
;;                      Parentheses and Region
;; --------------------------------------------------------------
;; enable show-paren-mode for emacs-lisp-mode
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init
  (setq show-paren-style 'parenthesis)
  :config
  (set-face-background 'show-paren-match (face-foreground 'warning))
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; rainbow parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; --------------------------------------------------------------
;;               Whitespace, Indentation and Delete
;; --------------------------------------------------------------

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; Visualize TAB, (HARD) SPACE, NEWLINE
(use-package whitespace
  :ensure nil
  :diminish
  :hook ((prog-mode outline-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-line-column fill-column ;; limit line length
        ;; automatically clean up bad whitespace
        ;; whitespace-action '(auto-cleanup)
        whitespace-action nil
        ;; only show bad whitespace
        whitespace-style '(face
                           trailing space-before-tab
                           indentation empty space-after-tab)))

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
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; --------------------------------------------------------------
;;                            Search
;; --------------------------------------------------------------

(use-package which-key
  :diminish (which-key-mode)
  :config (which-key-mode))

;; Installing Counsel will bring in Ivy and Swiper as dependencies.
;; counsel, swiper and ivy come from the same repo.
(use-package counsel
  :diminish (counsel-mode)
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  ;;(counsel-mode t)
  ;; Sort M-x commands by history
  (use-package amx)

  (use-package ivy
    :diminish
    :config
    (setq ivy-use-virtual-buffers t)
    (global-set-key (kbd "C-c C-r") 'ivy-resume))

  (use-package ivy-rich
    :hook (ivy-mode . ivy-rich-mode)
    :init
    (use-package all-the-icons-ivy-rich
      :init (all-the-icons-ivy-rich-mode 1))))

(use-package swiper
  :bind
  ("C-s" . swiper))

;; Highlight symbols
(use-package symbol-overlay
  :diminish
  :defines iedit-mode
  :commands (symbol-overlay-get-symbol
             symbol-overlay-assoc
             symbol-overlay-get-list
             symbol-overlay-jump-call)
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all)
         ([M-f3] . symbol-overlay-remove-all))
  :hook ((prog-mode . symbol-overlay-mode)
         (iedit-mode . (lambda () (symbol-overlay-mode -1)))
         (iedit-mode-end . symbol-overlay-mode)))

;; --------------------------------------------------------------
;;                           Completion
;; --------------------------------------------------------------

;; company is a useful auto-complete tool
(use-package company
  ;; `TODO' after setting init below, company is disabled under org mode, but
  ;; quickhelp is still working
  ;; :init (setq company-global-modes '(not org-mode))
  :diminish
  ;; `TODO' it seems like the hook is useless and company mode will always start
  ;; :hook (after-init . global-company-mode)
  ;; :hook (org-mode . (lambda () (company-mode -1)))
  :hook
  (prog-mode . company-mode)
  (LaTeX-mode . company-mode)
  :bind (("M-/" . company-complete)
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . my-company-yasnippet)
         ;; ("C-c C-y" . my-company-yasnippet)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  ;; :init
  ;; ;; show icons for company
  ;; (when (version<= "26" emacs-version)
  ;;   (use-package company-box
  ;;     :diminish (company-box-mode)
  ;;     :hook (company-mode . company-box-mode)))
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-idle-delay .2               ; decrease delay before autocompletion popup shows
        company-echo-delay 0                ; remove annoying blinking
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil))

;; --------------------------------------------------------------
;;                            Hightlight
;; --------------------------------------------------------------
;; highlight indentations
(use-package highlight-indent-guides
  :diminish
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive t)
  ;; Disable `highlight-indet-guides-mode' in `swiper'
  ;; https://github.com/DarthFennec/highlight-indent-guides/issues/40
  (with-eval-after-load 'ivy
    (defadvice ivy-cleanup-string (after my-ivy-cleanup-hig activate)
      (let ((pos 0) (next 0) (limit (length str)) (prop 'highlight-indent-guides-prop))
        (while (and pos next)
          (setq next (text-property-not-all pos limit prop nil str))
          (when next
            (setq pos (text-property-any next limit prop nil str))
            (ignore-errors
              (remove-text-properties next pos '(display nil face nil) str))))))))

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
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
