;; init-utils.el --- Utility configurations. -*- lexical-binding: t -*-

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
;;                           Project
;; --------------------------------------------------------------
(defun yilin/ibuffer-project-hook ()
  (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
  (unless (eq ibuffer-sorting-mode 'project-file-relative)
    (ibuffer-do-sort-by-project-file-relative)))

(use-package ibuffer-project
  :hook (ibuffer . yilin/ibuffer-project-hook))


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
  (:map elfeed-show-mode-map
        ("j" . next-line)
        ("k" . previous-line)
        ("h" . backward-char)
        ("l" . forward-char))
  :config
  (setq elfeed-search-filter "@6-months-ago")
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs))

;; --------------------------------------------------------------
;;                           Fancy Stuff
;; --------------------------------------------------------------

;; Youdao Dictionay
(use-package youdao-dictionary
  :bind (("C-c Y" . youdao-dictionary-search-at-point)
         ("C-c y" . youdao-dictionary-search-at-point-tooltip))
  :config
  (evil-set-initial-state 'youdao-dictionary-mode 'emacs)
  ;; Cache documents
  (setq url-automatic-caching t)
  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t))

(use-package eyebrowse
  :hook (after-init . eyebrowse-mode))

(use-package auto-save
  :quelpa
  (auto-save :repo "manateelazycat/auto-save" :fetcher github)
  :preface
  (defun my-auto-save-hook ()
    (require 'auto-save)
    (auto-save-enable))
  :hook (after-init . my-auto-save-hook)
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
