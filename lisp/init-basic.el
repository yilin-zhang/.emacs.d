;; init-basic.el --- Basic configurations. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                         Performance
;; --------------------------------------------------------------
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 3 1024 1024))

(defun yilin/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'yilin/display-startup-time)

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
;; disable ring-bell-function
(setq ring-bell-function 'ignore)

;; change yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default major-mode 'text-mode
              fill-column 80)

(setq-default truncate-lines t)

;; handle long lines
(global-so-long-mode 1)

(setq confirm-kill-emacs 'yes-or-no-p)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; --------------------------------------------------------------
;;                             Window
;; --------------------------------------------------------------
;; disable tool-bar-mode
(tool-bar-mode -1)

;; disable welcome screen
(setq inhibit-splash-screen 1)

;; --------------------------------------------------------------
;;                             Cursor
;; --------------------------------------------------------------
;; scroll one line at a time (less "jumpy" than defaults)
;; https://www.emacswiki.org/emacs/SmoothScrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1
      scroll-conservatively 100000) ;; keyboard scroll one line at a time

;; lock the cursor postion when scrolling up and down
(setq scroll-preserve-screen-position 'always)

;; make cursor jump to the newly created window
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

;; enable delete-selection-mode
(delete-selection-mode t)

;; highlight the current line
(global-hl-line-mode 1)

;; move the cursor to the newly created window
;; makes people easier to close it (C-g)
(use-package popwin
  :hook after-init)

;; --------------------------------------------------------------
;;                              Files
;; --------------------------------------------------------------

(setq make-backup-files nil  ; disable backup file
      auto-save-default nil  ; disable auto-save
      )

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

;; --------------------------------------------------------------
;;                            Custom
;; --------------------------------------------------------------

;; Full Screen with Mode Line Time Display

;; The setting of this variable must come before enable
;; display-time-mode, or it will not work.
(setq display-time-24hr-format 1
      display-time-string-forms
      '((propertize (concat 24-hours ":" minutes " ")
                    'face 'font-lock-constant-face)))


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

(global-set-key (kbd "<f12>") 'yilin/toggle-frame-fullscreen)
;; (yilin-toggle-frame-fullscreen)

;; Window/Frame

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

(global-set-key (kbd "C-x |") 'yilin/toggle-window-split)

;; --------------------------------------------------------------
;;                            Backup
;; --------------------------------------------------------------
;; inhibit the start message
;; (setq inhibit-startup-message t)

;; save desktop
;; (desktop-save-mode t)

;; press <f2> to open my init.el file
;;(defun open-init-file()
;;  (interactive)
;;  (find-file "~/.emacs.d/init.el"))
;;(global-set-key (kbd "<f2>") 'open-init-file)

(provide 'init-basic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
