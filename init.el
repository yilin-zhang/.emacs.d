;;; package --- My init.el
;;; Commentary:
;;; Code:

;; --------------------------------------------------------------
;;                          Pre-Setup
;; --------------------------------------------------------------
;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 3 1024 1024))
;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)

;; --------------------------------------------------------------
;;                            Paths
;; --------------------------------------------------------------
;; Move custom settings to a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Custom directories
(defvar yilin/site-lisp-directory
  (expand-file-name "site-lisp" user-emacs-directory) "Directory for local Emacs packages.")
(defvar yilin/config-lisp-directory
  (expand-file-name "lisp" user-emacs-directory) "Directory for local Emacs configuration.")
(defvar yilin/custom-lisp-directory
  (expand-file-name "custom" user-emacs-directory) "Directory for custom Emacs configuration.")

;; Add directories to load-path
(push yilin/config-lisp-directory load-path)
(when (file-directory-p yilin/custom-lisp-directory)
  (push yilin/custom-lisp-directory load-path))

;; --------------------------------------------------------------
;;                           Package
;; --------------------------------------------------------------

(require 'package)
(setq package-enable-at-startup nil
      package-native-compile t)

;; Configure package sources
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
;;(setq package-archives '(("gnu" . "https://elpa.emacs-china.org/gnu/")
;;                         ("melpa" . "https://elpa.emacs-china.org/melpa/")))

(package-initialize)

;; Should set before loading `use-package'
(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

(require 'use-package)

;; the key word ":diminish" only works when we have diminish
(use-package diminish)

;; --------------------------------------------------------------
;;                       Configurations
;; --------------------------------------------------------------
(require 'init-basic)
(require 'init-dired)
(require 'init-org)
(require 'init-git)
(require 'init-markup)
(require 'init-prog)
(require 'init-edit)
(require 'init-utils)

(when (file-directory-p yilin/custom-lisp-directory)
  (require 'custom-post))

(provide 'init)
;;; init.el ends here
