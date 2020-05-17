;; init-org.el --- Configurations for org mode. -*- lexical-binding: t -*-

(use-package org
  :ensure nil
  :preface
  ;; Copy from `https://emacs-china.org/t/org-agenda/8679'
  (defun my-org-agenda-time-grid-spacing ()
    "Set different line spacing w.r.t. time duration."
    (save-excursion
      (let* ((background (alist-get 'background-mode (frame-parameters)))
             (background-dark-p (string= background "dark"))
             (colors (if background-dark-p
                         (list "#aa557f" "DarkGreen" "DarkSlateGray" "DarkSlateBlue")
                       (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7")))
             pos
             duration)
        (nconc colors colors)
        (goto-char (point-min))
        (while (setq pos (next-single-property-change (point) 'duration))
          (goto-char pos)
          (when (and (not (equal pos (point-at-eol)))
                     (setq duration (org-get-at-bol 'duration)))
            (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                  (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
              (overlay-put ov 'face `(:background ,(car colors)
                                                  :foreground
                                                  ,(if background-dark-p "black" "white")))
              (setq colors (cdr colors))
              (overlay-put ov 'line-height line-height)
              (overlay-put ov 'line-spacing (1- line-height))))))))
  :bind
  ("C-c a" . org-agenda)
  ("C-c b" . org-switchb)
  ("C-c c" . org-capture)
  :hook
  (org-mode . org-indent-mode)
  (org-mode . auto-fill-mode)
  (org-mode . (lambda()(diminish 'org-indent-mode)))
  (org-mode . (lambda()(setq truncate-lines nil)))
  (org-agenda-finalize . my-org-agenda-time-grid-spacing)
  :config
  (setq org-log-done 'time           ; add time stamp after an item is DONE
        org-src-fontify-natively t   ; fontify code in code blocks
        org-startup-indented t       ; indent at startup
        org-hide-emphasis-markers t  ; hide emphasis markers
        org-pretty-entities t        ; make special character format visible
        org-ellipsis "⤵"
        org-image-actual-width nil   ; make org support image scaling
        )
  ;; Latex preview scale
  (setq org-export-backends '(ascii html icalendar latex md)
        org-format-latex-options (plist-put org-format-latex-options :scale 2.3)
        org-latex-compiler "xelatex") ; Set XeLaTeX as the default LaTeX compiler
  ;; Set my org agenda file
  (setq org-agenda-files '("~/agenda.org" "~/inbox.org")
        org-default-notes-file "~/inbox.org"
        org-capture-templates
        '(("l" "👨 Life" entry (file+headline org-default-notes-file "Life")
           "* TODO %?\n  %i\n")
          ("w" "📖 Work" entry (file+headline org-default-notes-file "Work")
           "* TODO %?\n %i\n %a")
          ("i" "🌏 Info" entry (file+headline org-default-notes-file "Info")
           "* %?\n%t\n")))
  ;; Set keywords properties
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)"
                                      "|" "DONE(d)" "CANCEL(c)"))
        org-todo-keyword-faces '(("HANGUP" . warning)))
  )

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package htmlize
  :after org)

;; literature management
;; path variables can be set in custom/custom-post.el
;; WORKAROUND: This package cannot be defered, otherwise the keybinding
;; won't be loaded until you run the command
(use-package org-ref
  :after org
  :init (require 'org-ref))

;; WORKAROUND: The same issue as above. It doesn't load unless explicitly
;; using `require' in `:init'
(use-package ox-hugo
  :after ox
  :init (require 'ox-hugo))

(use-package ox-jekyll-md
  :after ox
  :init (require 'ox-jekyll-md))

;; --------------------------------------------------------------
;;                            Custom
;; --------------------------------------------------------------

(defun yilin-org-insert-chinese-date-heading ()
  "Insert a Chinese date heading based on the current date."
  (interactive)
  (org-insert-heading-respect-content)
  (insert (format-time-string "%Y年"))
  (let ((month (format-time-string "%m"))
        (day (format-time-string "%d"))
        (week (format-time-string "%a")))
    (if (string-equal "0" (substring month 0 1))
        (setq month (substring month 1)))
    (if (string-equal "0" (substring day 0 1))
        (setq day (substring day 1)))
    (cond
     ((string-equal week "Mon") (setq week "一"))
     ((string-equal week "Tue") (setq week "二"))
     ((string-equal week "Wed") (setq week "三"))
     ((string-equal week "Thu") (setq week "四"))
     ((string-equal week "Fri") (setq week "五"))
     ((string-equal week "Sat") (setq week "六"))
     ((string-equal week "Sun") (setq week "日")))
    (insert (format "%s月%s日 %s" month day week))))

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
