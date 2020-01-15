;; init-utils.el --- Utility configurations. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;           Full Screen with Mode Line Time Display
;; --------------------------------------------------------------

;; The setting of this variable must come before enable
;; display-time-mode, or it will not work.
(setq display-time-default-load-average nil)
(setq display-time-24hr-format 1)

(defvar yilin-frame-fullscreen nil
  "Indicates whether the frame is toggled fullscreen or not.")
(defun yilin-toggle-frame-fullscreen ()
  "toggle-frame-fullscreen plus display-time-mode."
  (interactive)
  (toggle-frame-fullscreen)
  (if yilin-frame-fullscreen
      (progn (setq yilin-frame-fullscreen nil)
             (display-time-mode -1)
             (display-battery-mode -1))
    (progn (setq yilin-frame-fullscreen t)
           (display-time-mode 1)
           (display-battery-mode 1))))
(global-set-key (kbd "<f12>") 'yilin-toggle-frame-fullscreen)
(yilin-toggle-frame-fullscreen)

;; --------------------------------------------------------------
;;                         Window Split
;; --------------------------------------------------------------

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
;;                         Better Writting
;; --------------------------------------------------------------

;; center the paragraphs
;; A known bug: when linum-mode is enabled under org-mode, the screen margin
;; can be abnormal in certain cases. It might be caused by a bug of Emacs
;; before Emacs 26.1.
;; ref: https://github.com/rnkn/olivetti
;; So just setting linum-mode as a prog-mode-hook is a reasonable solution.
;; copy from Centaur Emacs
(use-package olivetti
  :ensure t
  :diminish visual-line-mode olivetti-mode
  :bind ("<f7>" . olivetti-mode)
  :init (setq olivetti-body-width 0.618))

;; ;; --------------------------------------------------------------
;;                           File Tree
;; --------------------------------------------------------------
(use-package neotree
  :ensure t
  ;; override evil-mode key bindings
  :hook (neotree-mode . (lambda ()
                          (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                          (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
                          (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                          (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                          (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
                          (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
                          (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
                          (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
                          (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))
  :config
  (global-set-key (kbd "<f8>") 'neotree-toggle)
  (setq neo-smart-open t)
  ;; requires all-the-icons
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; --------------------------------------------------------------
;;                           Projectile
;; --------------------------------------------------------------
(use-package projectile
  :ensure t
  :diminish
  :bind (:map projectile-mode-map
              ("s-t" . projectile-find-file) ; `cmd-t' or `super-t'
              ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix "")
  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t)
  :config
  (projectile-update-mode-line)
  (setq projectile-completion-system 'ivy))

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
  :ensure t
  :functions all-the-icons-octicon
  :hook ((ibuffer . (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic)))))
  :config
  (setq ibuffer-projectile-prefix
        (if (display-graphic-p)
            (concat
             (all-the-icons-octicon "file-directory"
                                    :face ibuffer-filter-group-name-face
                                    :v-adjust -0.04
                                    :height 1.1)
             " ")
          "Project: ")))

;; --------------------------------------------------------------
;;                           Fancy Stuff
;; --------------------------------------------------------------

;; try packages without actually install them
(use-package try
  :ensure t)

;; Youdao Dictionay
(use-package youdao-dictionary
  :ensure t
  :bind (("C-c y" . youdao-dictionary-search-at-point)
         ("C-c Y" . youdao-dictionary-search-at-point-tooltip))
  :config
  ;; Cache documents
  (setq url-automatic-caching t)
  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t))

;; --------------------------------------------------------------
;;                          3rd Party
;; --------------------------------------------------------------
(defun xah-open-in-terminal ()
  "Open the current dir in a new terminal window.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (let ((process-connection-type nil))
      (start-process "" nil "powershell" "start-process" "powershell"  "-workingDirectory" default-directory)))
   ((string-equal system-type "darwin")
    (let ((process-connection-type nil))
      (start-process "" nil "/Applications/iTerm.app/Contents/MacOS/iTerm2" default-directory)))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil))
      (start-process "" nil "x-terminal-emulator"
                     (concat "--working-directory=" default-directory))))))
(global-set-key (kbd "<f2>") 'xah-open-in-terminal)

;; --------------------------------------------------------------
;;                           Backup
;; --------------------------------------------------------------

;; (use-package smartparens
;; :ensure t
;; :diminish smartparens-mode
;; :hook
;; ((prog-mode text-mode outline-mode) . smartparens-mode)
;; :config
;; do not add another single quote under emacs-lisp-mode
;; (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
;; auto indent inside curly braces, and make the right brace be down below
;; (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
;; Solve the confliction between smartparens and hungry delete
;; Specifically, delete pair doesn't work when hungry delete is on.
;; The solution is found here:
;; https://github.com/syl20bnr/spacemacs/issues/6584
;; (defadvice hungry-delete-backward (before sp-delete-pair-advice activate)
;; (save-match-data (sp-delete-pair (ad-get-arg 0))))
;; )

(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
