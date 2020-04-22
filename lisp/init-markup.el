;; init-markup.el --- Configurations for markup languages. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                     LaTeX Mode Configurations
;; --------------------------------------------------------------
;; `TODO' I do not know what this means!
;; I have expanded this expr, and it shows that it downloads both
;; tex and auctex from elpa, but only requires tex.
;; It solves my problem that when I tried to (use-package auctex),
;; it told me that it could not require auctex.
;; The right macro below is stolen from exact use-package manual.
;; The relevant part of use-package manual is pasted below:
;; If you need to install a different package from the one named by use-package
(use-package tex
  :ensure auctex
  :config (set-default 'preview-scale-function 2.0))

;; Installing cdlatex requires a file call texmathp.el, but it comes with
;; auctex. It is wierd that the cdlatex's github pages says that file
;; is part of Emacs starting with the version 21.3, but my Emacs 25 obviously
;; doesn't have that. So I have to either download texmathp.el manually or
;; install auctex. I choose the latter, because I just want my package
;; management to be clear. Also, auctex is not bad, after all many people
;; consider it a better latex mode than the native latex mode.
(use-package cdlatex
  :hook
  (LaTeX-mode . cdlatex-mode)
  (LaTeX-mode . auto-fill-mode))

;; --------------------------------------------------------------
;;                     Markdown Mode Configurations
;; --------------------------------------------------------------

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (setq markdown-command "multimarkdown"))

;; --------------------------------------------------------------
;;                     Yaml Mode Configurations
;; --------------------------------------------------------------
(use-package yaml-mode)

;; --------------------------------------------------------------
;;                    LilyPond Mode Configurations
;; --------------------------------------------------------------
;; This package is in the system directory: /usr/share/emacs/site-lisp
(use-package lilypond-mode
  :load-path "/usr/share/emacs/site-lisp"
  :ensure nil
  :mode (("\\.ly$" . LilyPond-mode)
         ("\\.ily$" . LilyPond-mode))
  :config
  (setq LilyPond-pdf-command "atril"))

;; --------------------------------------------------------------
;;                     Gnuplot Mode Configurations
;; --------------------------------------------------------------
(use-package gnuplot-mode
  :mode ("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode))
;; specify the gnuplot executable (if other than /usr/bin/gnuplot)
;; (setq gnuplot-program "/sw/bin/gnuplot")

(provide 'init-markup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markup.el ends here
