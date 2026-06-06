;; init-completion.el --- In-buffer completion (corfu + capf). -*- lexical-binding: t -*-

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
  (corfu-preselect 'prompt)         ;; RET inserts your input; arrow down to pick a candidate
  (corfu-on-exact-match nil)        ;; don't auto-insert on an exact match
  (corfu-count 16)                  ;; show more candidates (default 10)
  ;; Stay open across orderless separators (insert one with M-SPC) so you
  ;; can refine in-buffer completion the same way as in the minibuffer.
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
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
  ;; Using :hook instead of :init so corfu actually defers -- :init
  ;; would force eager loading at startup.
  :hook ((after-init . global-corfu-mode)
         ;; Close the corfu popup when meow leaves insert state.
         (meow-insert-exit . corfu-quit)))

;; Show documentation / signature for the selected candidate in a popup
;; beside the completion list. (corfu built-in extension.)
(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.5 . 1.0)))

;; Sort candidates by how recently / often they were chosen, persisted
;; across sessions via savehist. (corfu built-in extension.)
(use-package corfu-history
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-history-mode)
  :config
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;; A custom package to show icons in corfu completion
;; Adapted from `'https://emacs-china.org/t/corfu-all-the-icons-icon/20907'
(use-package kind-nerd-icons
  :ensure nil
  :load-path yilin/site-lisp-directory
  ;; No :demand t -- :after already guarantees this loads once both
  ;; corfu and nerd-icons are loaded, at which point :config registers
  ;; the margin formatter. Forcing eager load is unnecessary.
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters
               #'kind-nerd-icons-margin-formatter))

;; Completion-at-point extensions: file paths, a dabbrev fallback, and
;; making the LSP/comint capfs composable with everything else.
(use-package cape
  :preface
  (defun yilin/cape-add-file-capf ()
    (add-hook 'completion-at-point-functions #'cape-file -10 t))
  (defun yilin/cape-add-dabbrev-capf ()
    (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t))
  :init
  (setq cape-dabbrev-check-other-buffers t)
  (add-hook 'prog-mode-hook #'yilin/cape-add-file-capf)
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook comint-mode-hook))
    (add-hook hook #'yilin/cape-add-dabbrev-capf))
  ;; eglot's capf is exclusive by default, which suppresses other capfs
  ;; (tempel, file, dabbrev) in LSP buffers. Wrap it (and friends) as
  ;; non-exclusive so they compose instead of shadowing each other.
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive))

(provide 'init-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
