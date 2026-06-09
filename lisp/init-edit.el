;; init-edit.el --- Configurations for a better editing experience. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                         Spell checker
;; --------------------------------------------------------------
(use-package jinx
  ;; Defer until first text/prog buffer instead of starting at
  ;; emacs-startup. Avoids spawning the enchant subprocess + loading
  ;; jinx eagerly when the user just opens Emacs to read the agenda.
  :hook ((text-mode . jinx-mode)
         (prog-mode . jinx-mode))
  :bind
  ("C-;" . jinx-correct)
  ("C-M-;" . jinx-languages))

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
    ;; Add the Tempel Capf to `completion-at-point-functions'.  `tempel-expand'
    ;; only triggers on exact matches. We add `tempel-expand' *before* the main
    ;; programming mode Capf, such that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions))
    ;; Alternatively use `tempel-complete' if you want to see all matches.  Use
    ;; a trigger prefix character in order to prevent Tempel from triggering
    ;; unexpectly.
    ;; (setq-local corfu-auto-trigger "/"
    ;;             completion-at-point-functions
    ;;             (cons (cape-capf-trigger #'tempel-complete ?/)
    ;;                   completion-at-point-functions))
    )
  (dolist (hook '(conf-mode-hook prog-mode-hook text-mode-hook))
    (add-hook hook 'tempel-setup-capf))
  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; NOTE: `global-tempel-abbrev-mode' is autoloaded -- calling it
  ;; directly in :init eagerly loads the whole tempel package at
  ;; startup. Deferring via :hook keeps the load lazy.
  :hook (after-init . global-tempel-abbrev-mode))

(use-package tempel-collection)

;; --------------------------------------------------------------
;;                      Parentheses and Region
;; --------------------------------------------------------------
;; enable show-paren-mode for emacs-lisp-mode
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-style 'mixed)
  :custom-face
  (show-paren-match ((t (:background ,(face-foreground 'warning))))))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; rainbow parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; --------------------------------------------------------------
;;         Whitespace, Indentation, Delete and Folding
;; --------------------------------------------------------------

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(use-package emacs
  :ensure nil
  :init
  (setq-default c-basic-offset 4
                tab-width 4
                indent-tabs-mode nil))

;; Visualize TAB, (HARD) SPACE, NEWLINE
;; `https://github.com/condy0919/emacs-newbie/blob/master/introduction-to-builtin-modes.md'
(use-package whitespace
  :ensure nil
  :hook (prog-mode . whitespace-mode)
  :custom
  (whitespace-line-column nil)
  (whitespace-style
   '(face                               ; visualize things below:
     empty                              ; empty lines at beginning/end of buffer
     ;; lines-tail                         ; lines go beyond `fill-column'
     space-before-tab                   ; spaces before tab
     trailing                           ; trailing blanks
     tabs                               ; tabs (show by face)
     tab-mark                           ; tabs (show by symbol)
     ))
  :config
  ;; Don't use different background for tabs.
  (face-spec-set 'whitespace-tab
                 '((t :background unspecified)))
  ;; Only use background and underline for long lines, so we can still have
  ;; syntax highlight.

  ;; For some reason use face-defface-spec as spec-type doesn't work.  My guess
  ;; is it's due to the variables with the same name as the faces in
  ;; whitespace.el.  Anyway, we have to manually set some attribute to
  ;; unspecified here.
  (face-spec-set 'whitespace-line
                 '((((background light))
                    :background "#d8d8d8" :foreground unspecified
                    :underline t :weight unspecified)
                   (t
                    :background "#404040" :foreground unspecified
                    :underline t :weight unspecified)))

  ;; Use softer visual cue for space before tabs.
  (face-spec-set 'whitespace-space-before-tab
                 '((((background light))
                    :background "#d8d8d8" :foreground "#de4da1")
                   (t
                    :inherit warning
                    :background "#404040" :foreground "#ee6aa7"))))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :hook ((lisp-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1))))))

;; Vertical indentation guides, like VSCode/Zed. Tree-sitter aware, so
;; the bars follow real scopes in `*-ts-mode' buffers (Python especially).
(use-package indent-bars
  :hook
  ;; Enable it only for Python for now.
  (python-ts-mode . indent-bars-mode)
  :custom
  (indent-bars-treesit-support t)
  ;; Bitmaps are inexplicably slow on macOS; character-based bars are
  ;; fast and look the same here.
  (indent-bars-prefer-character t)
  (indent-bars-starting-column 0)
  ;; Subtle, not the default rainbow -- a dim comment-colored line.
  (indent-bars-width-frac 0.15)
  (indent-bars-color-by-depth nil)
  (indent-bars-color '(font-lock-comment-face :face-bg nil :blend 0.425))
  (indent-bars-highlight-current-depth nil)
  ;; Extend bars across blank lines *within* an indented block only, so
  ;; there are no distracting gaps but no stray bars past the block end.
  (indent-bars-display-on-blank-lines 'least)
  :config
  ;; HACK: `indent-bars-display-on-blank-lines' puts a display property
  ;;   containing a newline on blank lines, which confuses `move-to-column'
  ;;   and breaks `next-line' past such lines (jdtsmith/indent-bars#22).
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
  :custom-face (hl-todo ((t (:box t :inherit))))
  :bind (:map hl-todo-mode-map
              ([C-f3] . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode)
  :config
  ;; Highlight ":" after the keyword too, like doom.
  (setq hl-todo-highlight-punctuation ":")
  ;; Semantic, distinctly-colored faces (instead of flattening everything
  ;; into just error/warning red+yellow). Borrowed from doom's keyword set
  ;; and extended with my own keywords.
  (setq hl-todo-keyword-faces
        '(;; needs doing later
          ("TODO" warning bold)
          ;; broken / unimplemented / actively wrong
          ("FIXME" error bold)
          ("BUG" error bold)
          ("DEFECT" error bold)
          ("ISSUE" error bold)
          ;; revisit / reconsider, not necessarily broken
          ("REVIEW" font-lock-keyword-face bold)
          ("WIP" font-lock-keyword-face bold)
          ;; intentional smell / questionable-but-works
          ("HACK" font-lock-constant-face bold)
          ("WORKAROUND" font-lock-constant-face bold)
          ("TRICK" font-lock-constant-face bold)
          ;; going away
          ("DEPRECATED" font-lock-doc-face bold)
          ;; informational aside
          ("NOTE" success bold))))

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
