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
  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
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
  :diminish
  :hook ((lisp-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1))))))

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
  (setf hl-todo-keyword-faces (assoc-delete-all "XXXX*" hl-todo-keyword-faces))
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK" "WIP"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

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

;; --------------------------------------------------------------
;;                            Custom
;; --------------------------------------------------------------

;; **************************************************************
;; Unfill paragraph (the reversed operation to auto-fill)
;; `https://www.emacswiki.org/emacs/UnfillParagraph'
;; **************************************************************
(use-package emacs
  :ensure nil
  :bind
  (("M-Q" . yilin/unfill-paragraph)
   :map ctl-x-map
   ("n" . yilin/narrow-or-widen-dwim)
   :map minibuffer-local-map
   ("M-<backspace>" . yilin/delete-minibuffer-directory))
  :preface
  (defun yilin/unfill-paragraph (&optional region)
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region)))

  (declare-function LaTeX-narrow-to-environment "latex" (&optional count))
  (defun yilin/narrow-or-widen-dwim (p)
    "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
    (declare (interactive-only t))
    (interactive "P")
    (cond ((and (buffer-narrowed-p) (not p)) (widen))
          ((region-active-p)
           (narrow-to-region (region-beginning)
                             (region-end)))
          ((derived-mode-p 'org-mode)
           ;; `org-edit-src-code' is not a real narrowing
           ;; command. Remove this first conditional if
           ;; you don't want it.
           (cond ((ignore-errors (org-edit-src-code) t)
                  (delete-other-windows))
                 ((ignore-errors (org-narrow-to-block) t))
                 (t (org-narrow-to-subtree))))
          ((derived-mode-p 'latex-mode)
           (LaTeX-narrow-to-environment))
          (t (narrow-to-defun))))

  (defun yilin/delete-minibuffer-directory ()
    "Delete the directory name before the last slash in the minibuffer."
    (interactive)
    (let* ((end (point))
           (beg (save-excursion
                  (when (eq ?/ (char-before))
                    (backward-char))
                  (if (search-backward "/" nil t)
                      (1+ (point))
                    (backward-word)
                    (point)))))
      (delete-region beg end)))

  (defun yilin/insert-timestamp ()
    (interactive)
    (insert (format-time-string "%Y%m%d%H%M%S")))

  (defun yilin/remove-empty-lines ()
    "Remove all empty lines (containing only whitespace) in the current buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (flush-lines "^[ \t]*$")))

  (defconst yilin/greek-math-symbol-map
    '(;; --- Greek letters (lowercase) ---
      ("alpha" . "α") ("beta" . "β") ("gamma" . "γ") ("delta" . "δ")
      ("epsilon" . "ε") ("zeta" . "ζ") ("eta" . "η") ("theta" . "θ")
      ("iota" . "ι") ("kappa" . "κ") ("lambda" . "λ") ("mu" . "μ")
      ("nu" . "ν") ("xi" . "ξ") ("omicron" . "ο") ("pi" . "π")
      ("rho" . "ρ") ("sigma" . "σ") ("tau" . "τ") ("upsilon" . "υ")
      ("phi" . "φ") ("chi" . "χ") ("psi" . "ψ") ("omega" . "ω")

      ;; --- Greek letters (uppercase) ---
      ("Alpha" . "Α") ("Beta" . "Β") ("Gamma" . "Γ") ("Delta" . "Δ")
      ("Epsilon" . "Ε") ("Zeta" . "Ζ") ("Eta" . "Η") ("Theta" . "Θ")
      ("Iota" . "Ι") ("Kappa" . "Κ") ("Lambda" . "Λ") ("Mu" . "Μ")
      ("Nu" . "Ν") ("Xi" . "Ξ") ("Omicron" . "Ο") ("Pi" . "Π")
      ("Rho" . "Ρ") ("Sigma" . "Σ") ("Tau" . "Τ") ("Upsilon" . "Υ")
      ("Phi" . "Φ") ("Chi" . "Χ") ("Psi" . "Ψ") ("Omega" . "Ω")

      ;; --- Logic ---
      ("forall" . "∀") ("exists" . "∃") ("nexists" . "∄")
      ("neg" . "¬") ("lnot" . "¬")
      ("land" . "∧") ("lor" . "∨")
      ("implies" . "⇒") ("iff" . "⇔")

      ;; --- Sets ---
      ("in" . "∈") ("notin" . "∉")
      ("subset" . "⊂") ("subseteq" . "⊆") ("nsubseteq" . "⊈")
      ("supset" . "⊃") ("supseteq" . "⊇") ("nsupseteq" . "⊉")
      ("emptyset" . "∅")
      ("cap" . "∩") ("cup" . "∪")
      ("propto" . "∝")

      ;; --- Relations ---
      ("=" . "=") ("!=" . "≠") ("equiv" . "≡") ("neq" . "≠")
      ("approx" . "≈") ("sim" . "∼") ("cong" . "≅")
      ("<=" . "≤") (">=" . "≥") ("<<" . "≪") (">>" . "≫")

      ;; --- Arithmetic ---
      ("times" . "×") ("cdot" . "·") ("ast" . "∗")
      ("pm" . "±") ("mp" . "∓")
      ("div" . "÷")
      ("sqrt" . "√") ("cbrt" . "∛") ("qdrt" . "∜")

      ;; --- Calculus & Algebra ---
      ("int" . "∫") ("iint" . "∬") ("iiint" . "∭")
      ("oint" . "∮") ("oiint" . "∯")
      ("sum" . "∑") ("prod" . "∏") ("lim" . "lim") ;; leave lim as text
      ("infty" . "∞") ("infinity" . "∞")
      ("nabla" . "∇") ("partial" . "∂")

      ;; --- Arrows ---
      ("->" . "→") ("=>" . "⇒") ("-->" . "⟶")
      ("<-" . "←") ("<--" . "⟵")
      ("<->" . "↔") ("<=>" . "⇔")
      ("uparrow" . "↑") ("downarrow" . "↓")
      ("Uparrow" . "⇑") ("Downarrow" . "⇓")
      ("leftrightarrow" . "↔") ("Rightarrow" . "⇒")
      ("Leftarrow" . "⇐") ("Leftrightarrow" . "⇔")

      ;; --- Geometry ---
      ("angle" . "∠") ("measuredangle" . "∡") ("perp" . "⊥") ("parallel" . "∥")

      ;; --- Miscellaneous ---
      ("degree" . "°") ("prime" . "′")
      ("ell" . "ℓ") ("hbar" . "ℏ") ("Re" . "ℜ") ("Im" . "ℑ")
      ("aleph" . "ℵ")
      ("top" . "⊤") ("bot" . "⊥"))
    "Alist mapping LaTeX-like names to Greek/math unicode symbols.
Used by `yilin/insert-greek-or-math-symbol'.")

  (defun yilin/insert-greek-or-math-symbol ()
    "Replace region with corresponding Greek or math symbol, or prompt for one.
If a region is active, trim whitespace around it and try to convert
its contents into the corresponding Greek or math symbol. If successful,
replace the region. If not, or if no region is active, prompt the user."
    (interactive)
    (let* ((text (when (use-region-p)
                   (string-trim (buffer-substring-no-properties
                                 (region-beginning) (region-end)))))
           (replacement (assoc-default text yilin/greek-math-symbol-map)))
      (if (and text replacement)
          ;; Replace region
          (progn
            (delete-region (region-beginning) (region-end))
            (insert replacement))
        ;; Prompt for input
        (let* ((name (completing-read "Greek letter: "
                                      (mapcar #'car yilin/greek-math-symbol-map)
                                      nil t))
               (symbol (assoc-default name yilin/greek-math-symbol-map)))
          (when symbol
            (when (use-region-p)
              (delete-region (region-beginning) (region-end)))
            (insert symbol))))))

  (defun yilin/insert-accented (accent char)
    "Insert CHAR with ACCENT. Uses combining marks when needed.
ACCENT choices: ^ ` ' ~ \" ."
    (interactive
     (list
      (read-char-choice "Accent (^ ` ' ~ \"): " '(?^ ?` ?' ?~ ?\" ?.))
      (read-char "Character: ")))
    (let* ((combining-map
            '((?^ . ?\u0302)   ;; COMBINING CIRCUMFLEX
              (?` . ?\u0300)   ;; COMBINING GRAVE
              (?' . ?\u0301)   ;; COMBINING ACUTE
              (?~ . ?\u0303)   ;; COMBINING TILDE
              (?\" . ?\u0308)  ;; COMBINING DIAERESIS
              (?. . ?\u0307))) ;; COMBINING DOT ABOVE
           (combining (cdr (assoc accent combining-map))))
      (insert (string char combining))))

  ;; **************************************************************
  ;; Quote lines
  ;; `http://xahlee.info/emacs/emacs/emacs_quote_lines.html'
  ;; **************************************************************
  (defun yilin/quote-lines ()
    "Wrap each line in the current block with quotes and a separator.
If a region is active, act on the region; otherwise act on the text
block delimited by blank lines.

For example,

  cat
  dog
  cow

becomes

  \"cat\",
  \"dog\",
  \"cow\",

or

  (cat)
  (dog)
  (cow)

If the delimiter is a left bracket, the closing delimiter is the
matching right bracket.

URL `http://ergoemacs.org/emacs/emacs_quote_lines.html'
Version 2017-01-08"
    (interactive)
    (let* (
           $p1
           $p2
           ($quoteToUse
            (read-string
             "Quote to use:" "\"" nil
             '(
               ""
               "\""
               "'"
               "("
               "{"
               "["
               )))
           ($separator
            (read-string
             "line separator:" "," nil
             '(
               ""
               ","
               ";"
               )))
           ($beginQuote $quoteToUse)
           ($endQuote
            ;; if begin quote is a bracket, set end quote to the matching one. else, same as begin quote
            (let (($syntableValue (aref (syntax-table) (string-to-char $beginQuote))))
              (if (eq (car $syntableValue) 4) ; ; syntax table, code 4 is open paren
                  (char-to-string (cdr $syntableValue))
                $quoteToUse
                ))))
      (if (use-region-p)
          (progn
            (setq $p1 (region-beginning))
            (setq $p2 (region-end)))
        (progn
          (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
              (progn (re-search-forward "\n[ \t]*\n")
                     (setq $p1 (point)))
            (setq $p1 (point)))
          (re-search-forward "\n[ \t]*\n" nil "NOERROR")
          (skip-chars-backward " \t\n")
          (setq $p2 (point))))
      (save-excursion
        (save-restriction
          (narrow-to-region $p1 $p2)
          (goto-char (point-min))
          (skip-chars-forward "\t ")
          (insert $beginQuote)
          (goto-char (point-max))
          (insert $endQuote)
          (goto-char (point-min))
          (while (re-search-forward "\n\\([\t ]*\\)" nil "NOERROR")
            (replace-match
             (concat $endQuote $separator (concat "\n" (match-string 1)) $beginQuote) "FIXEDCASE" "LITERAL"))
          ;;
          )))))

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
