;; init-org.el --- Configurations for org mode. -*- lexical-binding: t -*-

(require 'org)

;; --------------------------------------------------------------
;;                            Features
;; --------------------------------------------------------------

(setq org-log-done 'time          ; add time stamp after an item is DONE
      org-src-fontify-natively t  ; fontify code in code blocks
      org-startup-indented t      ; indent at startup
      org-hide-emphasis-markers t ; hide emphasis markers
      org-pretty-entities t       ; make special character format visible
      org-ellipsis "⤵"
      org-image-actual-width nil  ; make org support image scaling
      )

;; set org-indent-mode
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook '(lambda()(diminish 'org-indent-mode)))

;; automatically return
(add-hook 'org-mode-hook
          (lambda()
            (setq truncate-lines nil)))

;; auto-fill
(add-hook 'org-mode-hook 'auto-fill-mode)

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode))

(use-package htmlize
  :ensure t)

;; literature management
;; path variables can be set in custom/custom-post.el
(use-package org-ref
  :ensure t)

;; --------------------------------------------------------------
;;                 Markdown, LaTeX and Export
;; --------------------------------------------------------------
;; Latex preview scale
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.3))

;; suppport export markdown
;; original: (ascii html icalendar latex)
(setq org-export-backends
      (quote (ascii html icalendar latex md)))

;; Set XeLaTeX as the default LaTeX compiler
(setq org-latex-compiler "xelatex")

;; ox-hugo is an Org exporter backend that exports Org to Hugo-compatible
;; Markdown (Blackfriday) and also generates the front-matter
;; (in TOML or YAML format).
(use-package ox-hugo
  :ensure t
  :after ox)

;; --------------------------------------------------------------
;;                            Agenda
;; --------------------------------------------------------------
;; Set my org agenda file
(setq org-agenda-files '("~/agenda.org"))

;; For now I prefer refile manually.
;; (setq org-refile-targets '(("~/agenda.org" :maxlevel . 3)))

;; Set key binding for org agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)

;; Set keywords properties
(setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)"
                                    "|" "DONE(d)" "CANCEL(c)"))
      org-todo-keyword-faces '(("HANGUP" . warning)))

;; --------------------------------------------------------------
;;                            Capture
;; --------------------------------------------------------------
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/inbox.org")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("a" "Task for Today" checkitem (file+datetree org-default-notes-file)
         "[ ] %?")))

;; --------------------------------------------------------------
;;                            Customs
;; --------------------------------------------------------------
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

(add-hook 'org-agenda-finalize-hook #'my-org-agenda-time-grid-spacing)


(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
