;; init-basic.el --- Basic configurations. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                            Paths
;; --------------------------------------------------------------
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
;; --------------------------------------------------------------
;;                            Server
;; --------------------------------------------------------------
;; Start server
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

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

;; --------------------------------------------------------------
;;                             Window
;; --------------------------------------------------------------
;; disable tool-bar-mode
(tool-bar-mode -1)

;; disable scroll-bar-mode
(scroll-bar-mode -1)

;; disable menu-bar-mode
(menu-bar-mode -1)

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

;; Make the cursor have a tail, which is easier for
;; users to locate the cursor.
(use-package beacon
  :ensure t
  :diminish
  :hook (after-init . beacon-mode)
  :config
  (setq beacon-color 0.5)
  (setq beacon-size 70))

;; move the cursor to the newly created window
;; makes people easier to close it (C-g)
(use-package popwin
  :config (popwin-mode 1))

;; --------------------------------------------------------------
;;                             Line Numbers
;; --------------------------------------------------------------
;; show line number under prog-mode
;; if using emacs26, then use the second line,
;; or just use the first line.
;; (add-hook 'prog-mode-hook 'linum-mode)
;; (global-display-line-numbers-mode)
(if (version<= "26" emacs-version )
    (progn
      (setq display-line-numbers-type 'relative)
      (add-hook 'prog-mode-hook 'display-line-numbers-mode)
      )
  (add-hook 'prog-mode-hook 'linum-mode))

;; --------------------------------------------------------------
;;                             Terminal
;; --------------------------------------------------------------
;; (global-set-key (kbd "<f9>") 'ansi-term)

;; Shell Pop
(use-package shell-pop
  :bind ([f9] . shell-pop)
  :init
  (setq shell-pop-shell-type '("ansi-term" "*ansi-term*"
                               (lambda () (ansi-term shell-pop-term-shell)))))

;; --------------------------------------------------------------
;;                              Files
;; --------------------------------------------------------------

;; disable backup file
(setq make-backup-files nil)

;; disable auto-save
(setq auto-save-default nil)

;; enable recentf-mode
(use-package recentf
  :ensure nil
  :bind ("C-x C-r" . recentf-open-files)
  :init
  (setq recentf-max-saved-items 50)
  :config
  (recentf-mode 1))

;; --------------------------------------------------------------
;;                             Buffer
;; --------------------------------------------------------------
;; use ibuffer instead of CRM buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(use-package all-the-icons-ibuffer
  :init (all-the-icons-ibuffer-mode 1))

;; --------------------------------------------------------------
;;                          Icons and Emoji
;; --------------------------------------------------------------
(use-package all-the-icons)

;; support displaying emoji
(use-package emojify)
;; only display emoji for org mode
;; :hook (org-mode . emojify-mode))

;; --------------------------------------------------------------
;;                              Fonts
;; --------------------------------------------------------------
(if (display-graphic-p)
    (if (eq system-type 'darwin)
        (set-face-attribute
         'default nil :font "Sarasa Mono SC 16")
      (set-face-attribute
       'default nil :font "Sarasa Mono SC 13")))

;; --------------------------------------------------------------
;;                        Theme and Modeline
;; --------------------------------------------------------------
;; doom-themes-visual-bell-config must be loaded after setting
;; ring-bell-function, or it will not work.
(use-package doom-themes
  :init
  (load-theme 'doom-miramare t)
  (doom-themes-neotree-config)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq find-file-visit-truename t))

;; (use-package nyan-mode
;;   :hook (after-init . nyan-mode)
;;   :config
;;   ;; WORKAROUND (setq nyan-animate-nyancat t) seems useless, I don't
;;   ;; know why.
;;   (nyan-start-animation))

;; --------------------------------------------------------------
;;                            Dashboard
;; --------------------------------------------------------------

(use-package dashboard
  :diminish page-break-lines-mode
  :bind ([f5] . dashboard-refresh-buffer)
  :hook
  (after-init . dashboard-setup-startup-hook)
  :config
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; Set the footer
  (setq dashboard-banner-logo-title
        "Be kind to yourself, don't compare, don't expect too fast, and don't worry.")
  ;; Set the banner
  (let ((banner-image-path "~/.emacs.d/custom/banner.png"))
    (if (file-exists-p banner-image-path)
        (setq dashboard-startup-banner banner-image-path)
      (setq dashboard-startup-banner 3)))
  (setq dashboard-items '((agenda  . 20)
                          (bookmarks . 10)
                          (projects . 10))))

;; --------------------------------------------------------------
;;                            Custom
;; --------------------------------------------------------------

;; Full Screen with Mode Line Time Display

;; The setting of this variable must come before enable
;; display-time-mode, or it will not work.
(setq display-time-24hr-format 1)
(setq display-time-string-forms
      '((propertize (concat (all-the-icons-faicon "clock-o" :v-adjust 0.03) " " 24-hours ":" minutes " ")
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
(defun toggle-window-split ()
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

(global-set-key (kbd "C-x |") 'toggle-window-split)

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
