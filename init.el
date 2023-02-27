;;; package --- My init.el
;;; Commentary:
;;; Code:

;; Move custom settings to a separate file
(setq custom-file "~/.emacs.d/custom.el")

(require 'package)
(setq package-enable-at-startup nil)
(setq package-native-compile t)

;; Configure package sources

;;(add-to-list 'package-archives
;;       '(("gnu" . "http://elpa.emacs-china.org/gnu/")
;;         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

;;(setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
;;                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;; https://emacs-china.org/t/topic/2957/3
;;(define-advice use-package-ensure-elpa (:before (&rest r) fill-selected)
;;  "Make sure :ensure fill `package-selected-packages'."
;;  (add-to-list 'package-selected-packages (car r))
;;  (customize-save-variable 'package-selected-packages package-selected-packages))

(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(setq use-package-expand-minimally t)
(setq use-package-enable-imenu-support t)

(require 'use-package)

(use-package quelpa-use-package
  :init
  (require 'quelpa-use-package)
  (setq quelpa-update-melpa-p nil))

;; the key word ":diminish" only works when we have diminish
(use-package diminish)

;; ===================== REQUIRE INIT FILES ====================================
;; (setq load-path (append (list (expand-file-name
;; "/usr/share/emacs/site-lisp")) load-path))

(defun load-custom-post-file ()
  "Load custom-post file."
  (if (file-directory-p "~/.emacs.d/custom")
      (progn
        (push (expand-file-name "custom" user-emacs-directory) load-path)
        (load "custom-post"))))
(add-hook 'after-init-hook #'load-custom-post-file)

(push (expand-file-name "lisp" user-emacs-directory) load-path)

(require 'init-basic)
(require 'init-edit)
(require 'init-utils)
(require 'init-dired)
(require 'init-git)
(require 'init-org)
(require 'init-markup)
(require 'init-prog)

(provide 'init)
;;; init.el ends here
