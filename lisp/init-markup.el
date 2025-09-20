;; init-markup.el --- Configurations for markup languages. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                     LaTeX Mode Configurations
;; --------------------------------------------------------------
(use-package tex
  :ensure auctex
  :config (set-default 'preview-scale-function 2.0))

(use-package cdlatex
  :hook
  (LaTeX-mode . cdlatex-mode)
  (LaTeX-mode . auto-fill-mode)
  (LaTeX-mode . outline-minor-mode))

;; --------------------------------------------------------------
;;                     Typst Mode Configurations
;; --------------------------------------------------------------
(use-package typst-ts-mode
  :vc (:url "https://git.sr.ht/~meow_king/typst-ts-mode"))

(use-package ox-typst
  :after org)

;; --------------------------------------------------------------
;;                     Markdown Mode Configurations
;; --------------------------------------------------------------
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown")
  (markdown-fontify-code-blocks-natively t))

;; --------------------------------------------------------------
;;                     Mermaid Mode Configurations
;; --------------------------------------------------------------
(use-package mermaid-mode)

;; --------------------------------------------------------------
;;                     Data / Config
;; --------------------------------------------------------------
(use-package yaml-mode)

(use-package csv-mode)

(provide 'init-markup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markup.el ends here
