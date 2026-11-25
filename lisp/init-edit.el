;;; init-edit.el --- Configurations for a better editing experience. -*- lexical-binding: t -*-

;;; Commentary:
;; Configure spelling, templates, structural editing, whitespace, and other
;; editing conveniences.

;;; Code:

;; --------------------------------------------------------------
;;                         Spell checker
;; --------------------------------------------------------------
(use-package jinx
  :hook ((text-mode . jinx-mode)
         (prog-mode . jinx-mode))
  :bind
  ("C-;" . jinx-correct)
  ("C-M-;" . jinx-languages)
  :config
  ;; English dictionaries cannot validate Chinese text.
  (cl-pushnew "\\cc+" (alist-get t jinx-exclude-regexps) :test #'equal))

;; --------------------------------------------------------------
;;                         Template
;; --------------------------------------------------------------

(use-package tempel
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
         :map tempel-map
         ("<tab>" . tempel-next)
         ("<backtab>" . tempel-previous)
         ("RET" . tempel-done))

  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; `tempel-expand' only triggers on exact matches.  Put it before the
    ;; major mode's Capf so that it is tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions))
    ;; Alternatively, `tempel-complete' shows all matches.  A trigger prefix
    ;; keeps it from popping up unexpectedly.
    ;; (setq-local corfu-auto-trigger "/"
    ;;             completion-at-point-functions
    ;;             (cons (cape-capf-trigger #'tempel-complete ?/)
    ;;                   completion-at-point-functions))
    )
  (dolist (hook '(conf-mode-hook prog-mode-hook text-mode-hook))
    (add-hook hook 'tempel-setup-capf))
  ;; Not the global mode, which would load Tempel at startup.
  :hook ((conf-mode prog-mode text-mode) . tempel-abbrev-mode))

(use-package tempel-collection)

;; --------------------------------------------------------------
;;                      Parentheses and Region
;; --------------------------------------------------------------
(use-package paren
  :ensure nil
  :custom
  (show-paren-style 'mixed))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Rainbow parentheses for Lisp.  `lisp-data-mode' is the parent of the
;; Emacs Lisp and Common Lisp modes.
(use-package rainbow-delimiters
  :hook ((lisp-data-mode scheme-mode) . rainbow-delimiters-mode))

;; --------------------------------------------------------------
;;         Whitespace, Indentation, Delete and Folding
;; --------------------------------------------------------------

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(use-package emacs
  :ensure nil
  :init
  (setopt tab-width 4
          indent-tabs-mode nil))

;; Visualize TAB, (HARD) SPACE, NEWLINE.  Not enabled by default.
;; `https://github.com/condy0919/emacs-newbie/blob/master/introduction-to-builtin-modes.md'
(use-package whitespace
  :ensure nil
  :custom
  (whitespace-style
   '(face                               ; visualize things below:
     empty                              ; empty lines at beginning/end of buffer
     ;; lines-tail                         ; lines go beyond `fill-column'
     space-before-tab                   ; spaces before tab
     trailing                           ; trailing blanks
     tabs                               ; tabs (show by face)
     tab-mark                           ; tabs (show by symbol)
     )))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :commands aggressive-indent-mode
  :preface
  (defun yilin/maybe-enable-aggressive-indent ()
    "Enable `aggressive-indent-mode' unless the buffer is large."
    ;; It is slow in large files.
    ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
    (unless (> (buffer-size) (* 3000 80))
      (aggressive-indent-mode 1)))
  :hook ((lisp-mode . yilin/maybe-enable-aggressive-indent)
         (emacs-lisp-mode . yilin/maybe-enable-aggressive-indent)))

;; Vertical indentation guides.
(use-package indent-bars
  :commands indent-bars-mode
  :preface
  (defconst yilin/indent-bars-excluded-modes
    '(lisp-data-mode scheme-mode clojure-mode racket-mode fennel-mode hy-mode)
    "Lisp-family modes where indentation guides should stay disabled.")

  (defun yilin/maybe-enable-indent-bars ()
    "Enable `indent-bars-mode' unless this is a Lisp-family buffer."
    (unless (apply #'derived-mode-p yilin/indent-bars-excluded-modes)
      (indent-bars-mode 1)))
  :hook
  (prog-mode . yilin/maybe-enable-indent-bars)
  :custom
  (indent-bars-treesit-support t)
  ;; Bitmaps are slow on macOS.
  (indent-bars-prefer-character t)
  (indent-bars-starting-column 0)
  ;; A dim line in the comment color.
  (indent-bars-color-by-depth nil)
  (indent-bars-color '(font-lock-comment-face :face-bg nil :blend 0.425))
  (indent-bars-highlight-current-depth nil)
  ;; On blank lines, only draw bars inside an indented block.
  (indent-bars-display-on-blank-lines 'least)
  :config
  ;; HACK: `indent-bars-display-on-blank-lines' confuses `move-to-column',
  ;;   which breaks `next-line' (jdtsmith/indent-bars#22).
  (defun yilin/indent-bars--protect-move-to-column (fn col &rest args)
    (if-let* ((indent-bars-mode)
              (indent-bars-display-on-blank-lines)
              (nlp (line-end-position))
              (dprop (get-text-property nlp 'display))
              ((seq-contains-p dprop ?\n))
              ((> col (- nlp (point)))))
        (goto-char nlp)
      (apply fn col args)))
  (advice-add #'move-to-column :around #'yilin/indent-bars--protect-move-to-column))

;; Note that this package has conflict with smartparens.
;; The solution is written inside smartparens' configurations.
;; (use-package hungry-delete
;;   :diminish
;;   :hook (after-init . global-hungry-delete-mode)
;;   :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; --------------------------------------------------------------
;;                            Highlight
;; --------------------------------------------------------------
;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :custom
  ;; Highlight ":" after the keyword too.
  (hl-todo-highlight-punctuation ":")
  ;; Doom's keywords plus my own.
  (hl-todo-keyword-faces
   '(("TODO" warning bold)
     ("FIXME" error bold)
     ("BUG" error bold)
     ("DEFECT" error bold)
     ("ISSUE" error bold)
     ("REVIEW" font-lock-keyword-face bold)
     ("WIP" font-lock-keyword-face bold)
     ("HACK" font-lock-constant-face bold)
     ("WORKAROUND" font-lock-constant-face bold)
     ("TRICK" font-lock-constant-face bold)
     ("DEPRECATED" font-lock-doc-face bold)
     ("NOTE" success bold)))
  :bind (:map hl-todo-mode-map
              ([C-f3] . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode))

;; Make the cursor have a tail, which is easier for
;; users to locate the cursor.
(use-package pulsar
  :hook (after-init . pulsar-global-mode))

;; --------------------------------------------------------------
;;                            Undo
;; --------------------------------------------------------------
(use-package vundo)

;; --------------------------------------------------------------
;;                            Other
;; --------------------------------------------------------------
(use-package separedit
  :bind (:map prog-mode-map
              ("C-c '" . separedit))
  :hook (separedit-buffer-creation . auto-fill-mode)
  :custom
  (separedit-preserve-string-indentation t)
  (separedit-continue-fill-column t)
  )

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
