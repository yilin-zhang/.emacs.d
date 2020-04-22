;; init-utils.el --- Utility configurations. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;           Full Screen with Mode Line Time Display
;; --------------------------------------------------------------

;; The setting of this variable must come before enable
;; display-time-mode, or it will not work.
(setq display-time-24hr-format 1)
(setq display-time-string-forms
      '((propertize (concat (all-the-icons-faicon "clock-o" :v-adjust 0.03) " " 24-hours ":" minutes " ")
                    'face 'font-lock-constant-face)))

(defun yilin-toggle-frame-fullscreen ()
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

(global-set-key (kbd "<f12>") 'yilin-toggle-frame-fullscreen)
;; (yilin-toggle-frame-fullscreen)

;; --------------------------------------------------------------
;;                         Window/Frame
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
(use-package writeroom-mode
  :bind
  ("<f6>" . global-writeroom-mode)
  ("<f7>" . writeroom-mode)
  :config
  (add-to-list 'writeroom-major-modes 'org-mode)
  (setq writeroom-width 90))

;; ;; --------------------------------------------------------------
;;                           File Tree
;; --------------------------------------------------------------
(use-package neotree
  ;; override evil-mode key bindings
  :bind
  ("<f8>" . neotree-toggle)
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
  (setq neo-smart-open t)
  ;; requires all-the-icons
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; --------------------------------------------------------------
;;                           Projectile
;; --------------------------------------------------------------
(use-package projectile
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
;;                           Feed
;; --------------------------------------------------------------
(use-package elfeed
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-search-filter "")
  (evil-set-initial-state 'elfeed-search-mode 'emacs))

;; --------------------------------------------------------------
;;                           Fancy Stuff
;; --------------------------------------------------------------

;; try packages without actually install them
(use-package try)

;; Youdao Dictionay
(use-package youdao-dictionary
  :bind (("C-c Y" . youdao-dictionary-search-at-point)
         ("C-c y" . youdao-dictionary-search-at-point-tooltip))
  :config
  ;; Cache documents
  (setq url-automatic-caching t)
  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t))

;; epub reader
(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Noto Serif CJK SC"
                             :height 1.0))
  (add-hook 'nov-mode-hook 'my-nov-font-setup))

;; --------------------------------------------------------------
;;                          3rd Party
;; --------------------------------------------------------------
;; Copy from `https://www.emacswiki.org/emacs/UnfillParagraph'
(defun my-unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(define-key global-map (kbd "M-Q") 'my-unfill-paragraph)

;; Narrow and widen
;; Copy from `https://endlessparentheses.com/emacs-narrow-or-widen-dwim.html'
(defun my-narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only
;; copy it if that's what you want.
(define-key ctl-x-map "n" #'my-narrow-or-widen-dwim)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (define-key LaTeX-mode-map "\C-xn"
              nil)))
;; --------------------------------------------------------------
;;                           Backup
;; --------------------------------------------------------------

;; (use-package smartparens
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
