;; init-keys.el --- Modal editing and global key modifiers. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                            Key Bindings
;; --------------------------------------------------------------
;; Use M-l as backspace
;; (global-set-key (kbd "M-l") (kbd "<backspace>"))

;; Key Modifiers
(use-package emacs
  :ensure nil
  :init
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
               ([(super z)] . undo))))

(use-package fcitx
  :if (eq system-type 'gnu/linux)
  :init
  (fcitx-aggressive-setup)
  :config
  (setq fcitx-use-dbus t))

;; --------------------------------------------------------------
;;                            Meow
;; --------------------------------------------------------------

(use-package indent-shift
  :ensure nil
  :load-path yilin/site-lisp-directory
  :after meow)

(use-package surround-region
  :ensure nil
  :load-path yilin/site-lisp-directory
  :after meow)

(use-package meow
  ;; No :demand -- meow loads lazily. `meow-setup' below runs on
  ;; `after-init' and calls `(require 'meow)' explicitly, which pulls
  ;; in all sub-modules (meow-helpers, meow-core, ...) and fires
  ;; `(provide 'meow)', at which point `:config' runs and any
  ;; `:after meow' declarations in other modules fire too. We can't
  ;; rely on autoloads alone because the keymap helpers
  ;; (`meow-motion-define-key', `meow-leader-define-key', ...) are
  ;; defined in meow-helpers.el but not registered as autoloads.
  :preface
  (defun yilin/disable-meow ()
    "Turn `meow-mode' off in the current buffer.
Hook this onto modes (vterm, dape-repl, color-rg, ...) where
modal editing gets in the way."
    (meow-mode -1))

  (defun meow-setup ()
    (require 'meow)
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (setq meow-use-clipboard t)
    (meow-motion-define-key
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
     '("h" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet)
     ;; Surround (SPC SPC <char>)
     '("SPC" . surround-region-map)
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
     '("<" . indent-shift-left)
     '(">" . indent-shift-right)
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

  (defun yilin/toggle-meow-mode ()
    "Toggle meow-mode"
    (interactive)
    (if (bound-and-true-p meow-mode)
        (meow-mode -1)
      (meow-mode 1)))
  :hook
  ;; Should not put meow-setup under :config,
  ;; otherwise some leader bindings will not bind
  (after-init . meow-setup)
  :bind
  ("C-z" . yilin/toggle-meow-mode)
  :config
  (meow-global-mode 1))

(provide 'init-keys)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keys.el ends here
