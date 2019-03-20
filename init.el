;;; package --- My init.el
;;; Commentary:
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)

;; Configure package sources

;;(add-to-list 'package-archives
;;       '(("gnu" . "http://elpa.emacs-china.org/gnu/")
;;         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

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

;; the key word ":diminish" only works when we have diminish
(use-package diminish
  :ensure t)

;; ===================== REQUIRE INIT FILES ====================================
(setq load-path (append (list (expand-file-name
                   "/usr/share/emacs/site-lisp")) load-path))

(push (expand-file-name "lisp" user-emacs-directory) load-path)

(push (expand-file-name "site-lisp" user-emacs-directory) load-path)

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
   (quote
    (lsp-mode lsp-ui company-lsp flymake-diagnostic-at-point nyan-mode hl-todo symbol-overlay shell-pop ivy-rich gnuplot-mode yapfify amx highlight-indent-guides doom-modeline beacon ibuffer-projectile projectile pdf-tools youdao-dictionary wttrin magit dashboard diredfl doom-themes aggressive-indent company-box ox-hugo rust-mode popwin cdlatex auctex hungry-delete olivetti rainbow-delimiters expand-region emojify diff-hl pyvenv yasnippet markdown-mode org-bullets all-the-icons neotree avy swiper counsel company which-key try diminish use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "#46D9FF"))))
 '(diff-hl-delete ((t (:background "#ff6c6b"))))
 '(diff-hl-insert ((t (:background "#98be65"))))
 '(hl-todo ((t (:box t :inherit)))))

(provide 'init)
;;; init.el ends here
