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
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-width                   30
        treemacs-no-png-images           nil)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-project-follow-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-nerd-icons
  :after (treemacs nerd-icons)
  :custom-face
  (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
  (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
  :config (treemacs-load-theme "nerd-icons"))

(use-package treemacs-magit
  :after (treemacs magit)
  :hook ((magit-post-commit
          git-commit-post-finish
          magit-post-stage
          magit-post-unstage)
         . treemacs-magit--schedule-update))

;; --------------------------------------------------------------
;;                            Dashbord
;; --------------------------------------------------------------
(use-package dashboard
  :defer t
  :commands (dashboard-open)
  :init
  ;; add instruction
  (setq initial-scratch-message
        (concat initial-scratch-message
                ";; Press <f6> to open the dashboard\n\n"))
  ;; layout
  (setq dashboard-center-content t)
  ;; icons
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  :bind
  ("<f6>" . dashboard-open)
  :custom
  ;; minimalist layout
  (dashboard-startupify-list '(dashboard-insert-items))
  (dashboard-vertically-center-content t)
  ;; only show names, not paths
  (dashboard-agenda-prefix-format " %i %?-12t% s") ; same as the agenda format in `org-agenad-prefix-format'
  (dashboard-agenda-time-string-format "%m-%d %a") ; add an abbreviated weekday name
  (dashboard-bookmarks-item-format "%s") ; no need to show file paths
  (dashboard-recentf-show-base t)
  (dashboard-recentf-item-format "%s") ; only show file names, not whole paths
  ;; limited items
  (dashboard-items '((bookmarks . 10)
                     (projects  . 10)
                     (agenda    . 5)
                     (recents   . 5)
                     (registers . 5))))

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
  :commands yilin/denote-random-review)

(use-package denote
  :after org
  :init
  ;; `https://baty.net/posts/2022/11/keeping-my-org-agenda-updated/'
  (defvar yilin/denote-agenda-keyword "agenda"
    "Denote files with this keyword will be considered as agenda files")
  (defun yilin/denote-init-org-agenda-files ()
    "Append list of files containing `yilin/denote-agenda-keyword' to org-agenda-files"
    (interactive)
    (yilin/init-org-agenda-files)
    (let ((keyword (concat "_" yilin/denote-agenda-keyword)))
      (setq org-agenda-files
            (append org-agenda-files
                    (directory-files denote-directory t keyword)))))
  (yilin/denote-init-org-agenda-files))

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
(use-package trashcat
  :ensure nil
  :load-path yilin/site-lisp-directory
  :commands trashcat)

;; --------------------------------------------------------------
;;                         Terminal
;; --------------------------------------------------------------
(use-package vterm
  :after meow
  :commands vterm
  :hook
  (vterm-mode . (lambda () (meow-mode -1)))
  (vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
  )

(use-package multi-vterm
  :commands multi-vterm
  :after vterm)

(use-package vterm-toggle
  :commands vterm-toggle
  :bind ("s-j" . vterm-toggle))

;; --------------------------------------------------------------
;;                            Custom
;; --------------------------------------------------------------

;; **************************************************************
;; Open in finder and terminal (Mac)
;; **************************************************************
(use-package emacs
  :ensure nil
  :after meow
  :preface
  (defun yilin/expand-and-quote-default-directory ()
    (shell-quote-argument (expand-file-name default-directory)))

  (defun yilin/open-with-terminal ()
    "Open the current dir in a new iTerm window."
    (interactive)
    (shell-command (concat "open -a iTerm.app " (yilin/expand-and-quote-default-directory))))

  (defun yilin/open-with-finder-or-default-app ()
    "Open the current dir with Finder or open the current file with a default app"
    (interactive)
    (let ((dired-file (condition-case nil
                          (dired-get-filename)
                        (error nil))))
      (if dired-file
          (shell-command (concat "open " (shell-quote-argument dired-file)))
        (shell-command (concat "open " (yilin/expand-and-quote-default-directory))))))

  (defun yilin/-copy-file-path ()
    "Return the file path of the current buffer or file under cursor.
Returns nil if the buffer is not visiting a file and no file is under cursor."
    (cond
     ;; If buffer is visiting a file, return its path
     ((buffer-file-name) (buffer-file-name))
     ;; If in dired mode, return the full path of file under cursor
     ((eq major-mode 'dired-mode)
      (ignore-errors (dired-get-filename)))
     ;; Otherwise return nil
     (t nil)))

  (defun yilin/copy-file-path ()
    "Copy the current buffer file path to the clipboard."
    (interactive)
    (let ((filename (yilin/-copy-file-path)))
      (when filename
        (kill-new filename)
        (message "Copied buffer file path '%s' to the clipboard." filename))))

  (defun yilin/copy-file-name ()
    "Copy the current buffer file path to the clipboard."
    (interactive)
    (let ((path (yilin/-copy-file-path)))
      (if path
          (let ((filename (file-name-base path)))
            (kill-new filename)
            (message "Copied buffer file name '%s' to the clipboard." filename))
        (message "No file name available to copy."))))

  (defun yilin/lookup-thesaurus (&optional arg)
    "Look up the word on an online thesaurus.
If a region is active, use the text in that region (whitespaces stripped).
If no region is active, use the word at point.
If no word is at point, prompt for a word, using prefix arg as default if provided."
    (interactive "P")
    (let ((word
           (cond
            ;; If region is active, use the region text and strip whitespace
            ((use-region-p)
             (string-trim (buffer-substring-no-properties (region-beginning) (region-end))))
            ;; If there's a word at point, use it
            ((thing-at-point 'word)
             (thing-at-point 'word t))
            ;; Otherwise, prompt for a word
            (t
             (read-string "Word for thesaurus: " (when arg (format "%s" arg)))))))
      (browse-url (format "https://www.merriam-webster.com/thesaurus/%s" word))))

  (defun yilin/arxiv-get-paper-id ()
    "Get arXiv paper ID from user input or selected region.
Returns the paper ID as a trimmed string."
    (let* ((region-text (when (use-region-p)
                          (string-trim (buffer-substring-no-properties
                                        (region-beginning) (region-end)))))
           (looks-like-arxiv-id (when region-text
                                  (string-match-p "^\\([0-9]+\\.[0-9]+\\|[a-z-]+\\.[A-Z]+/[0-9]+\\)$" region-text)))
           (prompt (if looks-like-arxiv-id
                       (format "Enter arXiv paper ID (%s): " region-text)
                     "Enter arXiv paper ID: "))
           (user-input (read-string prompt)))
      (cond
       ;; If user just pressed enter and we have a valid region ID, use it
       ((and looks-like-arxiv-id (string-empty-p user-input))
        (when (use-region-p) (delete-region (region-beginning) (region-end)))
        region-text)
       ;; If user entered something, use that
       ((not (string-empty-p user-input))
        (when (use-region-p) (delete-region (region-beginning) (region-end)))
        (string-trim user-input))
       ;; If we have selected text that looks like an ID and user pressed enter
       (looks-like-arxiv-id
        (when (use-region-p) (delete-region (region-beginning) (region-end)))
        region-text)
       ;; Fallback: use whatever the user typed
       (t (string-trim user-input)))))

  (defun yilin/arxiv-insert-org-link (paper-id)
    "Insert an org-mode link for an arXiv paper given its ID.
If called interactively, prompts for paper ID or uses selected text.
PAPER-ID should be in format like '2301.07041' or 'math.GT/0309136'."
    (interactive (list (yilin/arxiv-get-paper-id)))
    (require 'url) ; simple lazy load
    (let* ((paper-id paper-id)
           (url (format "https://arxiv.org/abs/%s" paper-id))
           (title (condition-case nil
                      (let ((buffer (url-retrieve-synchronously url t nil 10)))
                        (when buffer
                          (unwind-protect
                              (with-current-buffer buffer
                                (goto-char (point-min))
                                (when (re-search-forward "<title>\\[.*?\\]\\s-*\\(.*?\\)</title>" nil t)
                                  (string-trim (match-string 1))))
                            (kill-buffer buffer))))
                    (error nil))))
      (insert (format "[[%s][%s]]" url (or title (format "arXiv:%s" paper-id))))
      (message "Inserted link for %s" (or title paper-id))))

  (defun yilin/arxiv-open-paper (paper-id)
    "Open an arXiv paper in the default browser.
If called interactively, prompts for paper ID or uses selected text.
PAPER-ID should be in format like '2301.07041' or 'math.GT/0309136'."
    (interactive (list (yilin/arxiv-get-paper-id)))
    (browse-url (format "https://arxiv.org/abs/%s" paper-id)))

  (defun yilin/org-insert-link-with-html-title (url)
    "Asynchronously fetch URL and insert an Org link with the page title at point."
    (interactive "sEnter URL: ")
    ;; Capture the buffer and point where we should insert later
    (let ((target-buf (current-buffer))
          (target-pos (point)))
      (url-retrieve
       url
       #'yilin/org--insert-link-callback
       (list url target-buf target-pos))))

  (defun yilin/org--insert-link-callback (status url target-buf target-pos)
    "Callback for `yilin/org-insert-link-with-title-async'.
STATUS is the retrieval status. URL is the original URL.
TARGET-BUF and TARGET-POS are where to insert the link."
    (require 'url)
    (require 'dom)
    (if (plist-get status :error)
        (message "Error fetching URL: %s" (plist-get status :error))
      (goto-char (point-min))
      ;; Skip HTTP headers
      (re-search-forward "\n\n" nil 'move)
      (let* ((dom (libxml-parse-html-region (point) (point-max)))
             (title-node (car (dom-by-tag dom 'title)))
             (title (when title-node (string-trim (dom-text title-node)))))
        ;; Clean up this temporary buffer
        (kill-buffer (current-buffer))
        ;; Insert into the original buffer at original position
        (when (buffer-live-p target-buf)
          (with-current-buffer target-buf
            (save-excursion
              (goto-char target-pos)
              (insert (if title
                          (format "[[%s][%s]]" url title)
                        (format "[[%s]]" url))))
            (message "Inserted link for %s" url))))))
  :config
  (meow-leader-define-key
   '("t" . yilin/open-with-terminal)
   '("e" . yilin/open-with-finder-or-default-app)))

(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
