;;; init-utils.el --- Utility configurations. -*- lexical-binding: t -*-

;;; Commentary:
;; Configure file trees, documentation, notes, buffers, terminals, and other
;; general-purpose utilities.

;;; Code:

;; --------------------------------------------------------------
;;                         File Tree
;; --------------------------------------------------------------
(use-package treemacs
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  :bind (([f8]        . treemacs)
         ("M-0"       . treemacs-select-window)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t b"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :preface
  (defun yilin/treemacs-set-indentation ()
    "Indent each level so that chevrons line up with the parent's icon.
A chevron and its tab span three columns in a GUI frame and two in a
terminal."
    (setq-local treemacs-indentation (if (display-graphic-p) 3 2)))

  (defun yilin/treemacs-cache-git-under-key (btn path buffer git-future)
    "Also cache PATH's git status under BTN's key.
Treemacs caches under the path but reads under the key, and the two
differ for collapsed directories."
    (when (and (eq treemacs--git-mode 'deferred) (buffer-live-p buffer))
      (with-current-buffer buffer
        (when-let* (((<= (point-min) btn (point-max)))
                    ((equal (treemacs-button-get btn :path) path))
                    (key (treemacs-button-get btn :key))
                    ((not (equal key path))))
          (ht-set! treemacs--git-cache key
                   (treemacs--get-or-parse-git-result git-future))))))
  :hook (treemacs-mode . yilin/treemacs-set-indentation)
  :config
  (advice-add 'treemacs--apply-annotations-deferred :before
              #'yilin/treemacs-cache-git-under-key)
  (setopt treemacs-missing-project-action  'remove
          treemacs-sorting                 'alphabetic-case-insensitive-asc
          treemacs-follow-after-init       t
          treemacs-width                   30
          ;; Skip the sidebar in `C-x o'.
          treemacs-is-never-other-window   t)
  ;; Refresh sooner than the default 2000ms.  Must be a float, because
  ;; treemacs divides it by 1000.  `setopt' would warn about the type.
  (setq treemacs-file-event-delay 500.0)
  (treemacs-project-follow-mode t))

;; On macOS, Emacs's file notifications miss changes to file contents, and
;; treemacs doesn't watch .git, so git faces go stale.  This uses fswatch
;; (`brew install fswatch') instead.
(use-package treemacs-fswatch
  :load-path yilin/site-lisp-directory
  :if (eq system-type 'darwin)
  :after treemacs
  :demand t
  :config (treemacs-fswatch-mode))

(use-package treemacs-nerd-icons
  :after nerd-icons
  :hook (treemacs-mode . treemacs-nerd-icons-config)
  :preface
  (defun yilin/treemacs-nerd-icons-pad-files ()
    "Shift file icons one column right to line up with folder icons.
The chevron of a folder is wider than a column, so its tab ends one stop
further."
    (let ((icons (treemacs-theme->gui-icons (treemacs--find-theme "nerd-icons"))))
      (maphash
       (lambda (key icon)
         (let ((display (get-text-property 0 'display icon)))
           (cond
            ((string-prefix-p " \t" icon)
             (puthash key (concat " " icon) icons))
            ((and (stringp display) (string-prefix-p " \t" display))
             (setq icon (copy-sequence icon))
             (put-text-property 0 1 'display (concat " " display) icon)
             (puthash key icon icons)))))
       icons)))
  :config
  (yilin/treemacs-nerd-icons-pad-files))

(use-package treemacs-magit
  :after (treemacs magit)
  :demand t)

;; --------------------------------------------------------------
;;                           Minimap
;; --------------------------------------------------------------
;; Patched copy of GNU ELPA's minimap.
(use-package minimap
  :load-path yilin/site-lisp-directory
  ;; Terminal can't show small characters in the minimap
  :if (display-graphic-p)
  :hook (after-init . minimap-mode)
  :custom
  ;; With a delay, the minimap freezes while scrolling.
  (minimap-update-delay 0)
  ;; Each minimap fontifies its whole buffer, about 0.1s per 100KB.
  (minimap-max-buffer-size 100000))

;; --------------------------------------------------------------
;;                         Better Writing
;; --------------------------------------------------------------
(use-package olivetti
  :bind
  ("<f7>" . olivetti-mode)
  :hook
  (org-mode . olivetti-mode)
  :custom
  ;; use a wider body width than default
  (olivetti-body-width (+ fill-column 5)))

;; --------------------------------------------------------------
;;                         Better Doc
;; --------------------------------------------------------------
(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command)
  ("C-h d" . helpful-at-point)
  ("C-h F" . helpful-function))

;; --------------------------------------------------------------
;;                        Note Taking
;; --------------------------------------------------------------

(use-package denote
  ;; `yilin/denote-agenda-files' needs it on the first `C-c a'.
  :warmup (denote)
  :commands yilin/denote-random-review
  :custom
  (denote-org-store-link-to-heading 'id)
  :preface
  (defun yilin/denote-random-review ()
    "Jump to a random location in Denote notes containing the :review: tag."
    (interactive)
    (require 'denote)
    (let ((files (denote-directory-files))
          matches)
      ;; Collect all occurrences of :review: across notes
      (dolist (f files)
        (with-temp-buffer
          (insert-file-contents f)
          (goto-char (point-min))
          (while (re-search-forward ":review:" nil t)
            (push (list f (line-number-at-pos) (point)) matches))))
      ;; Jump to a random match
      (if matches
          (let* ((choice (nth (random (length matches)) matches))
                 (file (nth 0 choice))
                 (line (nth 1 choice))
                 (pos  (nth 2 choice)))
            (find-file file)
            (goto-char pos)
            (message "Jumped to %s at line %d"
                     (file-name-nondirectory file) line))
        (message "No notes with :review: tag found."))))
  :init
  ;; `https://baty.net/posts/2022/11/keeping-my-org-agenda-updated/'
  (defvar yilin/denote-agenda-keyword "agenda"
    "Denote keyword used to identify notes that belong in `org-agenda-files'.")

  (defun yilin/denote-agenda-files ()
    "Return denote notes whose filename contains `yilin/denote-agenda-keyword'."
    (require 'denote)
    (let ((re (concat "_" (regexp-quote yilin/denote-agenda-keyword)
                      ".*\\.org\\'")))
      (when (file-directory-p denote-directory)
        (directory-files denote-directory t re))))

  (add-hook 'org-agenda-compose-functions #'yilin/denote-agenda-files))

(use-package annotate)

;; --------------------------------------------------------------
;;                           Project
;; --------------------------------------------------------------
(use-package ibuffer-project
  :preface
  (defun yilin/ibuffer-project-hook ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative)))
  :hook (ibuffer . yilin/ibuffer-project-hook))

(use-package eyebrowse
  :hook (after-init . eyebrowse-mode))

;; --------------------------------------------------------------
;;                           Info
;; --------------------------------------------------------------
(use-package info
  :hook (Info-mode . yilin/set-variable-pitch))

;; --------------------------------------------------------------
;;                           File
;; --------------------------------------------------------------
(use-package auto-save
  :vc (:url "https://github.com/manateelazycat/auto-save.git")
  :custom
  (auto-save-silent t)             ; quietly save
  :hook (after-init . auto-save-enable)
  :config
  ;; (setq auto-save-delete-trailing-whitespace t) ; automatically delete spaces at the end of the line when saving
  (setq auto-save-disable-predicates
        '((lambda ()
            (string-suffix-p
             "gpg"
             (file-name-extension (buffer-name)) t)))))

;; --------------------------------------------------------------
;;                            Calc
;; --------------------------------------------------------------
(use-package literate-calc-mode)

;; --------------------------------------------------------------
;;                            Sys Tools
;; --------------------------------------------------------------
(use-package macmount
  :load-path yilin/site-lisp-directory
  :if (eq system-type 'darwin)
  :commands (macmount-mount macmount-unmount macmount-eject))

;; --------------------------------------------------------------
;;                           Terminal
;; --------------------------------------------------------------
(use-package ghostel
  :vc (:url "https://github.com/dakra/ghostel"
       :lisp-dir "lisp"
       :rev :newest)
  :after meow
  :commands (ghostel ghostel-project ghostel-other)
  :bind ("s-j" . yilin/ghostel-popup-toggle)
  :hook
  (ghostel-mode . yilin/disable-meow)
  (ghostel-mode . (lambda () (hl-line-mode -1)))
  :preface
  (defun yilin/ghostel-popup-toggle ()
    "Toggle a ghostel terminal in a bottom window.
If the window is visible but not selected, select it."
    (interactive)
    (let ((win (seq-find
                (lambda (w)
                  (and (eq (window-parameter w 'window-side) 'bottom)
                       (with-current-buffer (window-buffer w)
                         (derived-mode-p 'ghostel-mode))))
                (window-list))))
      (cond
       ((and win (eq win (selected-window)))
        (delete-window win))
       (win
        (select-window win))
       (t
        (let* ((display-buffer-alist
                (cons '((lambda (buf _action)
                          (with-current-buffer buf
                            (derived-mode-p 'ghostel-mode)))
                        (display-buffer-in-side-window)
                        (side . bottom)
                        (slot . 0)
                        (window-height . 0.3)
                        (preserve-size . (nil . t)))
                      display-buffer-alist))
               (buf (or (seq-find (lambda (b)
                                    (with-current-buffer b
                                      (derived-mode-p 'ghostel-mode)))
                                  (buffer-list))
                        (ghostel))))
          (select-window (display-buffer buf))))))))

(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
