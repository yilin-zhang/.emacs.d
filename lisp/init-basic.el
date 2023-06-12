;; init-basic.el --- Basic configurations. -*- lexical-binding: t -*-

(require 'cl-lib)

;; --------------------------------------------------------------
;;                         Performance
;; --------------------------------------------------------------
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 3 1024 1024))

;; settings from Centaur Emcas
(use-package gcmh
  :hook after-init
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB

;; --------------------------------------------------------------
;;                            Paths
;; --------------------------------------------------------------
(use-package exec-path-from-shell
  :hook (after-init . (lambda ()
                        (when (memq window-system '(mac ns x))
                          (exec-path-from-shell-initialize)))))

;; --------------------------------------------------------------
;;                            Server
;; --------------------------------------------------------------
;; Start server
(use-package server
  :ensure nil
  :hook after-init)

;; --------------------------------------------------------------
;;                            Features
;; --------------------------------------------------------------
;; Set some basic features
(use-package emacs
  :ensure nil
  :init
  (tool-bar-mode -1)
  (setq inhibit-splash-screen 1) ; disable welcome screen
  ;; The setting of this variable must come before enable
  ;; display-time-mode, or it will not work.
  (setq display-time-24hr-format 1
        display-time-string-forms
        '((propertize (concat 24-hours ":" minutes " ")
                      'face 'font-lock-constant-face)))
  :hook
  (after-init . global-so-long-mode)
  (after-init . delete-selection-mode)
  (after-init . global-hl-line-mode) ; highlight the current line
  (after-init . pixel-scroll-precision-mode)
  (emacs-startup . yilin/display-startup-time)
  :config
  ;; Confirmation
  (fset 'yes-or-no-p 'y-or-n-p) ; change yes or no to y or n
  (setq confirm-kill-emacs 'yes-or-no-p)
  ;; Editing
  (setq-default major-mode 'text-mode
                fill-column 80)
  (setq-default truncate-lines t)
  (setq ring-bell-function 'ignore) ; disable ring-bell-function
  ;; Cursor and scrolling
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
  (setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ; scroll window under mouse
  (setq scroll-step 1
        scroll-conservatively 100000) ; keyboard scroll one line at a time
  (setq scroll-preserve-screen-position 'always) ; lock cursor position when scrolling
  ;; File saving
  (setq make-backup-files nil  ; disable backup file
        auto-save-default nil  ; disable auto-save
        )

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
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))

  (defun yilin/display-startup-time ()
    (message "Emacs loaded in %s with %d garbage collections."
             (format "%.2f seconds"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
             gcs-done))
  :bind
  ;; Focus on the new window after splitting
  ("C-x 2" . (lambda () (interactive) (split-window-vertically) (other-window 1)))
  ("C-x 3" . (lambda () (interactive) (split-window-horizontally) (other-window 1)))
  ("<f12>" . yilin/toggle-frame-fullscreen)
  ("C-x |" . yilin/toggle-window-split)
  )

(use-package popwin
  :hook after-init)

;; --------------------------------------------------------------
;;                          Buffers
;; --------------------------------------------------------------
;; Create some hooks for buffer related operations.
;; (Borrowed from Doom Emacs. Mainly for auto-revert.)

(defvar yilin/switch-buffer-hook nil
  "A list of hooks run after changing the current buffer.")

(defvar yilin/switch-window-hook nil
  "A list of hooks run after changing the focused windows.")

(defvar yilin/switch-frame-hook nil
  "A list of hooks run after changing the focused frame.")

(defun yilin/run-switch-buffer-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks 'yilin/switch-buffer-hook)))

(defun yilin/run-switch-window-or-frame-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (unless (equal (old-selected-frame) (selected-frame))
      (run-hooks 'yilin/switch-frame-hook))
    (unless (or (minibufferp)
                (equal (old-selected-window) (minibuffer-window)))
      (run-hooks 'yilin/switch-window-hook))))

(add-hook 'window-selection-change-functions #'yilin/run-switch-window-or-frame-hooks-h)
(add-hook 'window-buffer-change-functions #'yilin/run-switch-buffer-hooks-h)
(add-function :after after-focus-change-function #'yilin/run-switch-window-or-frame-hooks-h)

;;;###autoload
(defun yilin/visible-buffers (&optional buffer-list)
  "Return a list of visible buffers (i.e. not buried)."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list)))))
    (if buffer-list
        (cl-delete-if (lambda (b) (memq b buffer-list))
                      buffers)
      (delete-dups buffers))))

;; --------------------------------------------------------------
;;                          Auto Revert
;; --------------------------------------------------------------
;; The revert mechanism is borrowed from Doom Emacs.
;; All visible buffers are reverted immediately when
;; a) a file is saved or
;; b) Emacs is refocused (after using another app).

(use-package autorevert
  :ensure nil
  :diminish
  :hook
  (after-save . yilin/auto-revert-buffers-h)
  (yilin/switch-buffer . yilin/auto-revert-buffers-h)
  (yilin/switch-window . yilin/auto-revert-buffers-h)
  (yilin/switch-frame . yilin/auto-revert-buffers-h)
  :config
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil)

  (defun yilin/auto-revert-buffer-h ()
    "Auto revert current buffer, if necessary."
    (unless (or auto-revert-mode (active-minibuffer-window))
      (let ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun yilin/auto-revert-buffers-h ()
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (yilin/visible-buffers))
      (with-current-buffer buf
        (yilin/auto-revert-buffer-h)))))

;; --------------------------------------------------------------
;;                              Files
;; --------------------------------------------------------------
;; enable recentf-mode
(use-package recentf
  :ensure nil
  :hook after-init
  :custom
  (recentf-max-saved-items 50))

;; --------------------------------------------------------------
;;                          Icons and Emoji
;; --------------------------------------------------------------
(use-package nerd-icons)

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
(if (display-graphic-p)
    (if (eq system-type 'darwin)
        (set-face-attribute
         'default nil :font "Sarasa Mono SC 15")
      (set-face-attribute
       'default nil :font "Sarasa Mono SC 13")))

;; --------------------------------------------------------------
;;                        Theme and Modeline
;; --------------------------------------------------------------
;; doom-themes-visual-bell-config must be loaded after setting
;; ring-bell-function, or it will not work.
(use-package doom-themes
  :init
  (load-theme 'doom-gruvbox t)
  :config
  ;; Disable visual bell because it causes buffer selecting error
  ;; when it works with dired-async
  ;; (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :hook after-init
  :custom
  (doom-modeline-support-imenu t)
  (doom-modeline-modal-icon nil)
  (find-file-visit-truename t))


(provide 'init-basic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
