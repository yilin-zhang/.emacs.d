;; init-utils.el --- Utility configurations. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                         Terminal
;; --------------------------------------------------------------
(use-package vterm
  :commands vterm
  :hook (vterm-mode . (lambda ()
                        (setq-local global-hl-line-mode nil)
                        (meow-mode -1))))

(use-package multi-vterm
  :commands multi-vterm
  :after vterm)

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
;;                           Note Taking
;; --------------------------------------------------------------
(use-package denote)

;; --------------------------------------------------------------
;;                           Project
;; --------------------------------------------------------------
(defun yilin/ibuffer-project-hook ()
  (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
  (unless (eq ibuffer-sorting-mode 'project-file-relative)
    (ibuffer-do-sort-by-project-file-relative)))

(use-package ibuffer-project
  :hook (ibuffer . yilin/ibuffer-project-hook))

(use-package eyebrowse
  :hook (after-init . eyebrowse-mode))

;; --------------------------------------------------------------
;;                           Feed
;; --------------------------------------------------------------
(use-package elfeed
  :bind ("C-x w" . elfeed)
  (:map elfeed-search-mode-map
        ("j" . next-line)
        ("k" . previous-line)
        ("h" . backward-char)
        ("l" . forward-char))
  :config
  (setq elfeed-search-filter "@6-months-ago"))

;; --------------------------------------------------------------
;;                           Fancy Stuff
;; --------------------------------------------------------------

(use-package dictionary
  :ensure nil
  :config
  (setq dictionary-use-single-buffer t)
  (setq dictionary-server "dict.org"))

(use-package auto-save
  :ensure nil
  :quelpa (auto-save :repo "manateelazycat/auto-save"
                     :fetcher github)
  :hook (after-init . auto-save-enable)
  :config
  (setq auto-save-silent t)             ; quietly save
  ;; (setq auto-save-delete-trailing-whitespace t) ; automatically delete spaces at the end of the line when saving
  (setq auto-save-disable-predicates
        '((lambda ()
            (string-suffix-p
             "gpg"
             (file-name-extension (buffer-name)) t)))))

(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
