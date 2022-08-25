;;; package --- My init.el
;;; Commentary:
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)

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

;; ============================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(eglot json-mode company-web-html bicycle imenu-list kanji-mode auto-save elispfl quelpa-use-package rime eyebrowse yasnippet-snippets ox-jekyll-md elfeed org-superstar org-ref exec-path-from-shell all-the-icons-ivy-rich all-the-icons-ibuffer htmlize cmake-mode writeroom-mode whiteroom-mode yaml-mode web-mode js2-mode beacon flycheck evil-leader doom-modeline company-auctex evil-surround fcitx evil hl-todo symbol-overlay shell-pop ivy-rich gnuplot-mode amx highlight-indent-guides ibuffer-projectile projectile youdao-dictionary magit diredfl doom-themes aggressive-indent ox-hugo popwin cdlatex auctex rainbow-delimiters emojify diff-hl pyvenv yasnippet markdown-mode all-the-icons neotree swiper counsel company which-key try diminish use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:foreground "#fc20bb" :background nil))))
 '(diff-hl-delete ((t (:background nil))))
 '(diff-hl-insert ((t (:background nil))))
 '(hl-todo ((t (:box t :inherit)))))

(provide 'init)
;;; init.el ends here
