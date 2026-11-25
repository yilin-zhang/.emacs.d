;;; init-completion.el --- In-buffer completion (corfu + capf). -*- lexical-binding: t -*-

;;; Commentary:
;; Configure Corfu and completion-at-point functions for in-buffer completion.

;;; Code:

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                   ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                    ;; Enable auto completion
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-preselect 'prompt)         ;; nothing selected until TAB or an arrow key
  (corfu-count 16)                  ;; show more candidates (default 10)

  ;; Use TAB for cycling.
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  ;; Text buffers still have Dabbrev on `M-/'.
  :hook ((prog-mode . corfu-mode)
         ;; Close the corfu popup when meow leaves insert state.
         (meow-insert-exit . corfu-quit))
  :config
  ;; RET inserts the selected candidate, or acts as usual if none is selected.
  (keymap-set corfu-map "RET"
              `(menu-item "" corfu-insert
                          :filter ,(lambda (cmd)
                                     (and (>= corfu--index 0) cmd)))))

;; Show the documentation of the selected candidate beside the popup.
(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.5 . 1.0)))

;; Sort candidates by how recently and how often they were chosen.
(use-package corfu-history
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-history-mode))

;; Icons for corfu.  Adapted from `'https://emacs-china.org/t/corfu-all-the-icons-icon/20907'
(use-package kind-nerd-icons
  :load-path yilin/site-lisp-directory
  :after corfu
  :demand t
  :config
  (add-to-list 'corfu-margin-formatters
               #'kind-nerd-icons-margin-formatter))

;; More completion sources.
(use-package cape
  :preface
  (defun yilin/cape-add-file-capf ()
    (add-hook 'completion-at-point-functions #'cape-file -10 t))
  (defun yilin/cape-add-dabbrev-capf ()
    (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t))

  (defvar yilin/dabbrev-buffer-size-limit (* 1 1024 1024)
    "Skip other buffers larger than this many bytes when scanning for dabbrev.")

  (defun yilin/dabbrev-friend-buffer-p (other-buffer)
    "Non-nil if OTHER-BUFFER has the same major mode and isn't too large."
    (and (dabbrev--same-major-mode-p other-buffer)
         (< (buffer-size other-buffer) yilin/dabbrev-buffer-size-limit)))

  (defun yilin/cape-dabbrev-buffers ()
    "Return the current buffer and the others dabbrev allows scanning."
    (require 'dabbrev)
    (cons (current-buffer) (dabbrev--select-buffers)))
  :init
  ;; Cape doesn't apply dabbrev's filters to the buffers it scans.
  (setq cape-dabbrev-buffer-function #'yilin/cape-dabbrev-buffers)
  (add-hook 'prog-mode-hook #'yilin/cape-add-file-capf)
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook comint-mode-hook))
    (add-hook hook #'yilin/cape-add-dabbrev-capf))
  ;; These capfs are exclusive by default, which would hide the others.
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)
  ;; Don't scan large or internal buffers, TAGS tables and documents.
  (with-eval-after-load 'dabbrev
    (setopt dabbrev-friend-buffer-function #'yilin/dabbrev-friend-buffer-p
            dabbrev-ignored-buffer-regexps
            '("\\` "                      ; internal buffers
              "\\(?:\\(?:[EG]?\\|GR\\)TAGS\\|e?tags\\|GPATH\\)\\(<[0-9]+>\\)?")
            ;; An all-caps prefix matches case.
            dabbrev-upcase-means-case-search t)
    (dolist (mode '(pdf-view-mode doc-view-mode tags-table-mode))
      (add-to-list 'dabbrev-ignored-buffer-modes mode))))

(provide 'init-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
