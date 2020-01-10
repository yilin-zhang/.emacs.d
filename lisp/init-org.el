;; init-org.el --- Configurations for org mode. -*- lexical-binding: t -*-

(require 'org)

;; --------------------------------------------------------------
;;                            Features
;; --------------------------------------------------------------

(setq org-log-done 'time          ; add time stamp after an item is DONE
      org-src-fontify-natively t  ; fontify code in code blocks
      org-startup-indented t      ; indent at startup
      org-hide-emphasis-markers t ; hide emphasis markers
      org-pretty-entities t       ; makes special character format visible
      )

;; set org-indent-mode
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook '(lambda()(diminish 'org-indent-mode)))

;; automatically return
(add-hook 'org-mode-hook
          (lambda()
            (setq truncate-lines nil)))

(use-package org-bullets
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

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
(setq org-agenda-files '("~/agenda.org"
                         "~/inbox.org"))
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

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
