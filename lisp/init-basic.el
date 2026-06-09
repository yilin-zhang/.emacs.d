;; init-basic.el --- Basic configurations. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                         Performance
;; --------------------------------------------------------------
;; settings from Centaur Emacs
(use-package gcmh
  :hook emacs-startup
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB

;; Warm up heavy multi-file packages on idle time after startup, so
;; the first interactive use (e.g. `C-c a' for `org-agenda') doesn't
;; pause while a tower of sub-packages is `require'd synchronously.
;; Loading the package is enough -- it self-installs on
;; `emacs-startup-hook'. Consumers declare what to warm up via the
;; `:warmup' use-package keyword. See site-lisp/warmup.el.
(use-package warmup
  :ensure nil
  :load-path yilin/site-lisp-directory
  :demand t)

;; --------------------------------------------------------------
;;                        Basic Features
;; --------------------------------------------------------------
(use-package emacs
  :ensure nil
  :preface
  (defun yilin/display-startup-time ()
    (message "Emacs loaded in %s with %d garbage collections."
             (format "%.2f seconds"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
             gcs-done))
  :init
  ;; The setting of this variable must come before enable
  ;; display-time-mode, or it will not work.
  (setq display-time-24hr-format t
        display-time-string-forms
        '((propertize (concat 24-hours ":" minutes " ")
                      'face 'font-lock-constant-face)))
  ;; UI: window-divider defaults moved to early-init.el.
  ;; Confirmation
  (setq use-short-answers t) ; Emacs 28+: replaces (fset 'yes-or-no-p 'y-or-n-p)
  (setq confirm-kill-emacs 'yes-or-no-p)
  ;; Editing
  (setq-default major-mode 'text-mode
                fill-column 80
                truncate-lines t)
  ;; ring-bell-function moved to early-init.el.
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  ;; Cursor and scrolling
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ; one line at a time
        mouse-wheel-progressive-speed nil            ; don't accelerate scrolling
        mouse-wheel-follow-mouse 't                  ; scroll window under mouse
        scroll-step 1
        scroll-conservatively 100000                 ; keyboard scroll one line at a time
        scroll-preserve-screen-position 'always      ; lock cursor position when scrolling
        )
  ;; Frame: frame-resize-pixelwise moved to early-init.el.
  ;; File saving
  (setq make-backup-files nil  ; disable backup file
        auto-save-default nil  ; disable auto-save
        )
  ;; Resolve symlinks when visiting files, so buffers always point at
  ;; the canonical path (also keeps doom-modeline's buffer name honest).
  (setq find-file-visit-truename t)
  ;; Warning handling
  (setq warning-suppress-types '((emacs)))
  ;; Misc QoL tweaks (from emacsredux "stealing from the best configs").
  (setq set-mark-command-repeat-pop t  ; keep popping the mark ring with C-SPC
        help-window-select t           ; focus help/helpful windows when they open
        ffap-machine-p-known 'reject   ; never ping hostnames on find-file-at-point
        reb-re-syntax 'string)         ; readable string syntax in `re-builder'
  :hook
  (after-init . global-so-long-mode)
  (after-init . delete-selection-mode)
  (after-init . global-hl-line-mode) ; highlight the current line
  (after-init . pixel-scroll-precision-mode)
  ;; Make saved files that start with a shebang executable automatically.
  (after-save . executable-make-buffer-file-executable-if-script-p)
  (window-setup . window-divider-mode)
  (emacs-startup . yilin/display-startup-time)
  )

;; --------------------------------------------------------------
;;                              Icons
;; --------------------------------------------------------------
;; No :demand -- doom-modeline (:hook after-init) requires nerd-icons,
;; pulling it in at that point.  Other consumers (nerd-icons-ibuffer,
;; treemacs-nerd-icons, kind-nerd-icons) rely on their :after chains.
(use-package nerd-icons)

;; --------------------------------------------------------------
;;                        Theme and Modeline
;; --------------------------------------------------------------
;; doom-themes-visual-bell-config must be loaded after setting
;; ring-bell-function, or it will not work.
(use-package doom-themes
  :demand t
  :config
  (load-theme 'doom-gruvbox t)
  ;; Disable visual bell because it causes buffer selecting error
  ;; when it works with dired-async
  ;; (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :hook after-init
  :custom
  (doom-modeline-support-imenu t)
  (doom-modeline-modal-icon nil))

;; --------------------------------------------------------------
;;                            Paths
;; --------------------------------------------------------------
(use-package exec-path-from-shell
  ;; Only relevant in GUI sessions; TTY/batch inherit the parent
  ;; shell's environment already. `:if' is evaluated at use-package
  ;; expansion time, so the whole block is dropped on non-GUI Emacs.
  :if (memq window-system '(mac ns x))
  :init
  ;; The default `exec-path-from-shell-arguments' is ("-l" "-i"), but
  ;; `-i' forces loading ~/.zshrc / ~/.bashrc, which typically runs
  ;; slow plugin init (nvm, rbenv, oh-my-zsh, conda init, ...) and can
  ;; cost 100-500ms at startup. `-l' alone still reads ~/.zprofile /
  ;; ~/.zlogin / /etc/profile, which by convention is where env
  ;; variables belong. Put env vars in .zprofile, not .zshrc.
  (setq exec-path-from-shell-arguments '("-l"))
  :hook (after-init . exec-path-from-shell-initialize))

;; --------------------------------------------------------------
;;                            Server
;; --------------------------------------------------------------
(use-package server
  :ensure nil
  :hook after-init)

;; --------------------------------------------------------------
;;                            Window
;; --------------------------------------------------------------
(use-package emacs
  :preface
  ;; Full Screen with Mode Line Time Display
  (defun yilin/toggle-frame-fullscreen ()
    "toggle-frame-fullscreen plus display-time-mode."
    (interactive)
    (if (equal 'fullboth (frame-parameter nil 'fullscreen))
        (progn
          (display-time-mode -1)
          (display-battery-mode -1))
      (progn
        (display-time-mode 1)
        (display-battery-mode 1)))
    (toggle-frame-fullscreen))

  ;; Set key-binding for switching from a horizontal
  ;; split to a vertical split and vice versa.
  (defun yilin/toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-right
                  'split-window-below)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))
  :bind
  ;; Focus on the new window after splitting
  ([remap split-window-right] . (lambda () (interactive) (split-window-right) (other-window 1)))
  ([remap split-window-below] . (lambda () (interactive) (split-window-below) (other-window 1)))
  ("<f12>" . yilin/toggle-frame-fullscreen)
  ("C-x |" . yilin/toggle-window-split)
  )

(use-package popwin
  :hook after-init)

;; --------------------------------------------------------------
;;                          Auto Revert
;; --------------------------------------------------------------
(use-package lazy-revert
  :load-path yilin/site-lisp-directory
  :preface
  (defun yilin/revert-all-buffers ()
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (let ((auto-revert-mode t))
          (auto-revert-handler)))))
  :hook after-init
  :config
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        ;; Only prompts for confirmation when buffer is unsaved.
        revert-without-query (list ".")))

;; --------------------------------------------------------------
;;                              Files
;; --------------------------------------------------------------
;; enable recentf-mode
(use-package recentf
  :ensure nil
  :hook after-init
  :custom
  (recentf-max-saved-items 50))

;; Remember and restore point's position in each visited file.
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode)
  :config
  ;; Recenter the window after save-place jumps to the restored position.
  (advice-add 'save-place-find-file-hook :after
              (lambda (&rest _)
                (when buffer-file-name (ignore-errors (recenter))))))

;; --------------------------------------------------------------
;;                             Buffer
;; --------------------------------------------------------------
;; use ibuffer instead of CRM buffer
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

(use-package nerd-icons-ibuffer
  :after ibuffer
  :hook ibuffer-mode)

;; --------------------------------------------------------------
;;                              Fonts
;; --------------------------------------------------------------

(use-package emacs
  :ensure nil
  :preface
  (defvar yilin/default-font-size 15 "The default font size")
  (defvar yilin/fixed-pitch-font "Sarasa Term SC" "The fixed pitch font")
  (defvar yilin/variable-pitch-font "Noto Serif" "The fixed pitch font")

  (defun yilin/set-fonts (&optional font-size)
    "Set fonts with the given FONT-SIZE."
    (interactive (list (read-number "Enter size: " yilin/default-font-size)))
    (let ((actual-font-size (if (and font-size (< 0 font-size))
                                font-size
                              yilin/default-font-size)))
      (set-face-attribute 'default nil :font (font-spec :family yilin/fixed-pitch-font :size actual-font-size))
      (set-face-attribute 'fixed-pitch nil :font (font-spec :family yilin/fixed-pitch-font :size actual-font-size))
      (set-face-attribute 'variable-pitch nil :font (font-spec :family yilin/variable-pitch-font :size (+ 2 actual-font-size)))
      ;; Nerd Font fallback for PUA glyphs (nerd-icons, markview callouts, etc.)
      (when (find-font (font-spec :family "Symbols Nerd Font Mono"))
        (set-fontset-font t 'symbol "Symbols Nerd Font Mono" nil 'append)
        (set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'append))))

  (defun yilin/set-variable-pitch ()
    "Set current buffer's font to variable-pitch"
    (buffer-face-set 'variable-pitch))

  :init
  (when (display-graphic-p)
    (yilin/set-fonts)))

(provide 'init-basic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
