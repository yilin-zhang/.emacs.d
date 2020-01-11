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

;; the key word ":diminish" only works when we have diminish
(use-package diminish
  :ensure t)

;; ===================== REQUIRE INIT FILES ====================================
;; (setq load-path (append (list (expand-file-name
;; "/usr/share/emacs/site-lisp")) load-path))

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
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(package-selected-packages
   (quote
    (dap-mode company-lsp lsp-ui lsp-mode w3m evil-leader doom-modeline flycheck rubocop racket-mode company-auctex evil-surround fcitx evil lispy pdf-tools hl-todo symbol-overlay shell-pop ivy-rich gnuplot-mode yapfify amx highlight-indent-guides ibuffer-projectile projectile youdao-dictionary wttrin magit dashboard diredfl doom-themes aggressive-indent ox-hugo popwin cdlatex auctex hungry-delete olivetti rainbow-delimiters expand-region emojify diff-hl pyvenv yasnippet markdown-mode org-bullets all-the-icons neotree swiper counsel company which-key try diminish use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "#46D9FF"))))
 '(diff-hl-delete ((t (:background "#ff6c6b"))))
 '(diff-hl-insert ((t (:background "#98be65"))))
 '(hl-todo ((t (:box t :inherit))))
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic))))))

(provide 'init)
;;; init.el ends here
