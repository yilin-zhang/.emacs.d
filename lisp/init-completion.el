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

(provide 'init-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
