;; init-utils.el --- Utility configurations. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                         File Tree
;; --------------------------------------------------------------
(use-package treemacs
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-git-mode)
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  ;; doom-gruvbox leaves `treemacs-git-untracked-face' completely
  ;; unstyled, so untracked files look identical to normal ones. Give
  ;; them a visible (green) face, consistent with "new" elsewhere.
  (treemacs-git-untracked-face ((t (:inherit font-lock-string-face))))
  :bind (([f8]        . treemacs)
         ("M-0"       . treemacs-select-window)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t b"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        ;; Case-insensitive so `Zebra' doesn't sort before `apple'.
        treemacs-sorting                 'alphabetic-case-insensitive-asc
        treemacs-follow-after-init       t
        treemacs-width                   30
        ;; Skip the sidebar when cycling windows (`C-x o' etc.) so the
        ;; cursor never lands in the file tree by accident.
        treemacs-is-never-other-window   t
        treemacs-no-png-images           nil)
  ;; Continuous follow-mode (auto-locating the current file on every
  ;; buffer switch) is too jarring -- the tree keeps jumping around.
  ;; `treemacs-follow-after-init' still does a one-time locate on open,
  ;; and `treemacs-project-follow-mode' keeps the tree scoped to the
  ;; current project, which is the part worth keeping.
  (treemacs-follow-mode -1)
  (treemacs-filewatch-mode t)
  (treemacs-project-follow-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-nerd-icons
  :after nerd-icons
  ;; Defer icon registration until treemacs is actually opened.
  ;; Was on `after-init', which configured icons at every startup
  ;; even when treemacs was never used.
  :hook (treemacs-mode . treemacs-nerd-icons-config))

(use-package treemacs-magit
  :after (treemacs magit)
  :hook ((magit-post-commit
          git-commit-post-finish
          magit-post-stage
          magit-post-unstage)
         . treemacs-magit--schedule-update))

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
  ;; Pre-load denote on idle so the first `org-agenda-compose-refresh'
  ;; doesn't pay the `(require 'denote)' cost on the user's first
  ;; `C-c a'. (The agenda refresh advice still re-scans the directory
  ;; on every invocation -- that's I/O, not package loading.)
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
    "Return denote notes whose filename contains `yilin/denote-agenda-keyword'.
Intended to be registered on `org-agenda-compose-functions'."
    (require 'denote)
    (let ((re (concat "_" (regexp-quote yilin/denote-agenda-keyword)
                      ".*\\.org\\'")))
      (directory-files denote-directory t re)))

  ;; Register denote as a dynamic source for `org-agenda-files'. The
  ;; actual scan happens lazily inside `org-agenda-compose-refresh'
  ;; (defined in the org-agenda-compose package), which runs on org
  ;; load and before every `org-agenda' invocation.
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
  :hook (after-init . auto-save-enable)
  :config
  (setq auto-save-silent t)             ; quietly save
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
  :ensure nil
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
    "Toggle a ghostel terminal in a bottom popup window (VSCode-style).
If the popup is selected, dismiss it.  If it is visible elsewhere,
focus it.  Otherwise, show an existing ghostel buffer (or create
one) in a bottom side window."
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
