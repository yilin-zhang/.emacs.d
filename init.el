;;; package --- My init.el
;;; Commentary:
;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)

;; Move custom settings to a separate file
(setq custom-file "~/.emacs.d/custom.el")

(require 'package)
(setq package-enable-at-startup nil
      package-native-compile t)

;; Configure package sources
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "http://melpa.org/packages/")))
;;(setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
;;                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

(require 'use-package)

(use-package quelpa-use-package
  :init
  (setq quelpa-update-melpa-p nil)
  :demand t)

;; the key word ":diminish" only works when we have diminish
(use-package diminish)

;; ===================== REQUIRE INIT FILES ====================================

(push (expand-file-name "lisp" user-emacs-directory) load-path)

(require 'init-basic)
(require 'init-edit)
(require 'init-utils)
(require 'init-dired)
(require 'init-git)
(require 'init-org)
(require 'init-markup)
(require 'init-prog)

(when (file-directory-p "~/.emacs.d/custom")
  (push (expand-file-name "custom" user-emacs-directory) load-path)
  (require 'custom-post))

(provide 'init)
;;; init.el ends here
