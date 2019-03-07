;; init-org.el --- Configurations for org mode. -*- lexical-binding: t -*-

(require 'org)

;; add time stamp after an item is DONE
(setq org-log-done 'time)

;; fontify code in code blocks
(setq org-src-fontify-natively t)

;; indent at startup
(setq org-startup-indented t)

;; hide emphasis markers
(setq org-hide-emphasis-markers t)

;; makes special character format visible
(setq org-pretty-entities t)

;; suppport export markdown
;; original: (ascii html icalendar latex)
(setq org-export-backends
      (quote (ascii html icalendar latex md)))

(setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)"
				    "|" "DONE(d)" "CANCEL(c)")))
;; set org-indent-mode
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook '(lambda()(diminish 'org-indent-mode)))

;; Org agenda settings

;; Set my org agenda file
;; '(org-agenda-files (quote ("~/todolist.org")))
(setq org-agenda-files '("~/agenda.org"))

;; Set key binding for org agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)

;; Latex preview scale
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.3))

;; Set XeLaTeX as the default LaTeX compiler
(setq org-latex-compiler "xelatex")

(use-package org-bullets
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;; automatically return
(add-hook 'org-mode-hook
	  (lambda()
	    (setq truncate-lines nil)))

;; ox-hugo is an Org exporter backend that exports Org to Hugo-compatible
;; Markdown (Blackfriday) and also generates the front-matter
;; (in TOML or YAML format).
(use-package ox-hugo
  :ensure t
  :after ox)

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
