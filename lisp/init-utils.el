;; init-utils.el --- Utility configurations. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;           Full Screen with Mode Line Time Display
;; --------------------------------------------------------------

;; The setting of this variable must come before enable
;; display-time-mode, or it will not work.
(setq display-time-default-load-average nil)
(setq display-time-24hr-format 1)

(defvar my/frame-fullscreen nil
  "Indicates whether the frame is toggled fullscreen or not.")

(defun my/toggle-frame-fullscreen ()
  "toggle-frame-fullscreen plus display-time-mode."
  (interactive)
  (toggle-frame-fullscreen)
  (if my/frame-fullscreen
      (progn (setq my/frame-fullscreen nil)
             (display-time-mode -1))
    (progn (setq my/frame-fullscreen t)(display-time-mode 1))))

(global-set-key (kbd "<f11>") 'my/toggle-frame-fullscreen)

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
  :diminish visual-line-mode olivetti-mode
  :bind ("<f7>" . olivetti-mode)
  :init (setq olivetti-body-width 0.618))

;; --------------------------------------------------------------
;;                           PDF Reading
;; --------------------------------------------------------------

;; ;; `TODO' o is not binded to pdf-outline, which is weird,
;; ;; so I force it to be binded.
;; (use-package pdf-tools
;;   :ensure t
;;   :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
;;   :hook
;;   (pdf-view-mode . pdf-links-minor-mode)
;;   (pdf-view-mode . pdf-misc-context-menu-minor-mode)
;;   (pdf-view-mode . pdf-outline-minor-mode)
;;   (pdf-view-mode . pdf-annot-minor-mode)
;;   (pdf-view-mode . (lambda()(line-number-mode -1)))
;;   :bind (:map pdf-view-mode-map
;;               ("C-s" . pdf-occur)
;;               ("o" . pdf-outline))
;;   :config (pdf-tools-install))

(when (display-graphic-p)
  (use-package pdf-view
    :ensure pdf-tools
    :diminish (pdf-view-midnight-minor-mode pdf-view-printer-minor-mode)
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
    :magic ("%PDF" . pdf-view-mode)
    :preface
    (defun my-pdf-set-midnight-colors ()
      (setq pdf-view-midnight-colors
            `(,(face-foreground 'default) . ,(face-background 'default))))

    ;; Workaround for pdf-tools not reopening to last-viewed page
    ;; https://github.com/politza/pdf-tools/issues/18
    (defun my-pdf-set-last-viewed-bookmark ()
      (interactive)
      (when (eq major-mode 'pdf-view-mode)
        (bookmark-set (my-pdf-generate-bookmark-name))))

    (defun my-pdf-jump-last-viewed-bookmark ()
      (when (my-pdf-has-last-viewed-bookmark)
        (bookmark-jump (my-pdf-generate-bookmark-name))))

    (defun my-pdf-has-last-viewed-bookmark ()
      (assoc
       (my-pdf-generate-bookmark-name) bookmark-alist))

    (defun my-pdf-generate-bookmark-name ()
      (concat "LAST-VIEWED: " (buffer-name)))

    (defun my-pdf-set-all-last-viewed-bookmarks ()
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (my-pdf-set-last-viewed-bookmark))))
    :bind (:map pdf-view-mode-map
                ("C-s" . pdf-occur))
    :hook ((after-load-theme . my-pdf-set-midnight-colors)
           (kill-buffer . my-pdf-set-last-viewed-bookmark)
           (pdf-view-mode . my-pdf-jump-last-viewed-bookmark)
           (kill-emacs . (lambda ()
                           (unless noninteractive  ; as `save-place-mode' does
                             (my-pdf-set-all-last-viewed-bookmarks))))
           ;; TODO enable line number might cause Emacs freeze
           (pdf-view-mode . (lambda()(linum-mode -1)))
           (pdf-view-mode . (lambda()(line-number-mode -1))))
    :init (my-pdf-set-midnight-colors)
    :config (pdf-tools-install t nil t t)))
;; --------------------------------------------------------------
;;                           File Tree
;; --------------------------------------------------------------
(use-package neotree
  :ensure t
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

;; awesome-tab requires projectile
(use-package awesome-tab
  :load-path "site-lisp"
  :ensure nil
  :after projectile
  :bind ("<f6>" . awesome-tab-mode)
  :config
  (setq awesome-tab-style "box")
  (setq awesome-tab-background-color "#1D1F21")
  (setq awesome-tab-display-sticky-function-name nil))

;; --------------------------------------------------------------
;;                           Fancy Stuff
;; --------------------------------------------------------------

;; try packages without actually install them
(use-package try
  :ensure t)

;; check weather information
(use-package wttrin
  :ensure t
  :commands (wttrin)
  :init
  (setq wttrin-default-cities '("西安" "杭州")))

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
