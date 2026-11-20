;;; gruvbox-jb-theme.el --- Gruvbox as the JetBrains plugin draws it -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; A port of the "Gruvbox Theme" JetBrains plugin (Lonre Wang,
;; github.com/lonre/gruvbox-intellij-theme, 0.7.1) to doom-themes.
;; The UI faces come from doom-themes' doom-gruvbox (MIT); the syntax
;; and editor colors follow the plugin's Gruvbox Dark color schemes.
;;
;; Every color is from the gruvbox palette.  The few the plugin adds
;; of its own (brace and doc-comment colors, tinted backgrounds for
;; search and symbol highlights) and the ones doom-gruvbox adds are
;; moved to the nearest gruvbox color, stepping to the next nearest
;; where that would match a background it has to stand out from.  The
;; plugin's schemes sit on top of Darcula; nothing is taken from there
;; but the Rust colors below, which get the same treatment.
;;
;; Emacs gets one face per category where JetBrains can tell languages
;; apart, so the Rust and Python colors below are drawn per buffer.
;; `font-lock-preprocessor-face' takes DEFAULT_METADATA's aqua, which
;; fits Rust attributes and macros but also colors C's #include.

;;; Code:

(require 'doom-themes)

(defcustom gruvbox-jb-variant 'medium
  "Background contrast: `hard', `medium' or `soft', as in the plugin.
The plugin's three dark schemes differ only in background."
  :type '(choice (const hard) (const medium) (const soft))
  :group 'doom-themes)

;;;; Rust

;; RustRover colors Rust items through its own Darcula additions, which
;; the plugin inherits: enums, structs, traits, macros, `self' and so
;; on.  These faces carry those colors, moved to the nearest gruvbox
;; color.  They are remapped onto eglot's semantic token faces in Rust
;; buffers only, since other languages color the same token types
;; differently (CLion's structs stay yellow, for one).

(defface gruvbox-jb-rust-enum '((t)) "Rust enums.")
(defface gruvbox-jb-rust-enum-member '((t)) "Rust enum variants.")
(defface gruvbox-jb-rust-struct '((t)) "Rust structs and unions.")
(defface gruvbox-jb-rust-type-alias '((t)) "Rust type aliases.")
(defface gruvbox-jb-rust-trait '((t)) "Rust traits, derives included.")
(defface gruvbox-jb-rust-macro '((t)) "Rust macro calls.")
(defface gruvbox-jb-rust-type-param '((t)) "Rust type parameters.")
(defface gruvbox-jb-rust-lifetime '((t)) "Rust lifetimes.")
(defface gruvbox-jb-rust-self '((t)) "Rust `self'.")
(defface gruvbox-jb-rust-trait-method '((t)) "Rust trait method calls.")

(defconst gruvbox-jb-rust-remaps
  '((eglot-semantic-enum         . gruvbox-jb-rust-enum)
    (eglot-semantic-enumMember   . gruvbox-jb-rust-enum-member)
    (eglot-semantic-struct       . gruvbox-jb-rust-struct)
    (eglot-semantic-union        . gruvbox-jb-rust-struct)
    (eglot-semantic-typeAlias    . gruvbox-jb-rust-type-alias)
    (eglot-semantic-interface    . gruvbox-jb-rust-trait)
    (eglot-semantic-derive       . gruvbox-jb-rust-trait)
    (eglot-semantic-macro        . gruvbox-jb-rust-macro)
    (eglot-semantic-macroBang    . gruvbox-jb-rust-macro)
    (eglot-semantic-typeParameter . gruvbox-jb-rust-type-param)
    (eglot-semantic-lifetime     . gruvbox-jb-rust-lifetime)
    (eglot-semantic-selfKeyword  . gruvbox-jb-rust-self)
    (eglot-semantic-trait        . gruvbox-jb-rust-trait-method))
  "Eglot semantic faces and the Rust faces drawn over them.")

(defun gruvbox-jb-setup-rust-faces ()
  "Color this buffer's Rust semantic tokens the way RustRover does.
Meant for `rust-mode-hook'."
  (pcase-dolist (`(,face . ,rust-face) gruvbox-jb-rust-remaps)
    (face-remap-add-relative face rust-face)))

;;;; Python

;; The plugin's PY.* colors.  `python-ts-mode' shares one face between
;; decorators, class names and type hints, and one between `self' and
;; the keywords, so those get tree-sitter rules of their own; def names
;; and keyword arguments have a face to themselves and are remapped.

(defface gruvbox-jb-python-definition '((t)) "Python def and class names.")
(defface gruvbox-jb-python-decorator '((t)) "Python decorators.")
(defface gruvbox-jb-python-self '((t)) "Python `self' and `cls'.")
(defface gruvbox-jb-python-keyword-argument '((t))
  "Python keyword argument names in calls.")

(defun gruvbox-jb-setup-python-faces ()
  "Color this buffer's Python the way PyCharm does.
Meant for `python-ts-mode-hook'."
  (face-remap-add-relative 'font-lock-function-name-face
                           'gruvbox-jb-python-definition)
  (face-remap-add-relative 'font-lock-property-name-face
                           'gruvbox-jb-python-keyword-argument)
  (setq-local treesit-font-lock-settings
              (append treesit-font-lock-settings
                      (treesit-font-lock-rules
                       :language 'python
                       :feature 'definition
                       :override t
                       '((class_definition
                          name: (identifier) @gruvbox-jb-python-definition))
                       :language 'python
                       :feature 'decorator
                       :override t
                       '((decorator "@" @gruvbox-jb-python-decorator)
                         (decorator [(identifier) (attribute)]
                                    @gruvbox-jb-python-decorator)
                         (decorator (call function: [(identifier) (attribute)]
                                          @gruvbox-jb-python-decorator)))
                       :language 'python
                       :feature 'keyword
                       :override t
                       '(((identifier) @gruvbox-jb-python-self
                          (:match "\\`\\(?:self\\|cls\\)\\'"
                                  @gruvbox-jb-python-self))))))
  ;; Turn the new rules on per `treesit-font-lock-level'.
  (treesit-font-lock-recompute-features))

;;;; Theme

(def-doom-theme gruvbox-jb
  "Gruvbox Dark, colored like the JetBrains Gruvbox plugin."
  :family 'gruvbox-jb
  :background-mode 'dark

  ;; name        gui       256       16
  ((bg         (pcase gruvbox-jb-variant
                 ('hard '("#1d2021" "#1d2021" nil))   ; bg0_h
                 ('soft '("#32302f" "#32302f" nil))   ; bg0_s
                 (_     '("#282828" "#282828" nil)))) ; bg0
   ;; gruvbox has nothing darker than bg0_h for the hard variant
   (bg-alt     (pcase gruvbox-jb-variant
                 ('soft '("#282828" "#282828" nil))
                 (_     '("#1d2021" "#1d2021" nil))))
   (bg-alt2    '("#504945" "#504945" "brown"      )) ; bg2

   (base0      '("#1d2021" "black"   "black"      )) ; bg0_h
   (base1      '("#1d2021" "#1d1d1d" "brightblack")) ; bg0_h
   (base2      '("#282828" "#282828" "brightblack")) ; bg0
   (base3      '("#3c3836" "#383838" "brightblack")) ; bg1
   (base4      '("#665c54" "#5c5c5c" "brightblack")) ; bg3
   (base5      '("#7c6f64" "#6f6f6f" "brightblack")) ; bg4
   (base6      '("#928374" "#909090" "brightblack")) ; gray
   (base7      '("#d5c4a1" "#cccccc" "brightblack")) ; fg2
   (base8      '("#fbf1c7" "#fbfbfb" "brightwhite")) ; fg0
   (fg         '("#ebdbb2" "#dfdfdf" "brightwhite")) ; fg1
   (fg-alt     '("#d5c4a1" "#cccccc" "brightwhite")) ; fg2
   (fg-hard    '("#f9f5d7" "#fbfbfb" "brightwhite")) ; light0_hard

   (grey        '("#928374" "#909090" "brightblack"))
   (red         '("#fb4934" "#e74c3c" "red"))
   (magenta     '("#b16286" "#b16286" "magenta"))
   (violet      '("#d3869b" "#d3869b" "brightmagenta"))
   (orange      '("#fe8019" "#fd971f" "orange"))
   (dark-orange '("#d65d0e" "#d65d0e" "orange"))
   (yellow      '("#fabd2f" "#fabd2f" "yellow"))
   (dark-yellow '("#d79921" "#fabd2f" "yellow"))
   (teal        '("#8ec07c" "#8ec07c" "green"))
   (green       '("#b8bb26" "#b8bb26" "green"))
   (dark-green  '("#98971a" "#98971a" "green"))
   (blue        '("#83a598" "#83a598" "brightblue"))
   (dark-blue   '("#458588" "#458588" "blue"))
   (cyan        '("#8ec07c" "#8ec07c" "brightcyan"))
   (dark-cyan   '("#689d6a" "#689d6a" "cyan"))

   ;; face categories
   (highlight      yellow)
   (vertical-bar   bg-alt2)
   (selection      bg-alt2)
   (builtin        orange)               ; PY.BUILTIN_NAME
   (comments       grey)
   ;; doom's base theme also uses this for treemacs icons and other
   ;; UI text; doom-gruvbox's light #e0d2b8, nearest gruvbox color.
   ;; Doc comments themselves get their color below.
   (doc-comments   fg)
   (constants      violet)
   (functions      teal)                 ; DEFAULT_FUNCTION_DECLARATION
   (keywords       red)
   (methods        teal)                 ; DEFAULT_INSTANCE_METHOD
   (operators      fg)                   ; DEFAULT_OPERATION_SIGN
   (type           yellow)
   (strings        green)
   (variables      blue)
   (numbers        violet)
   (region         bg-alt2)              ; doom-gruvbox's; the plugin's is base4
   (error          red)
   (warning        yellow)
   (success        green)

   (vc-modified    (doom-darken cyan 0.15))
   (vc-added       (doom-darken green 0.15))
   (vc-deleted     (doom-darken red 0.15))

   ;; custom categories
   (-modeline-pad
    (when doom-themes-padded-modeline
      (if (integerp doom-themes-padded-modeline)
          doom-themes-padded-modeline
        4)))
   (modeline-bg bg-alt2)
   (modeline-fg (doom-lighten fg-alt 0.25))
   (modeline-inactive-bg (doom-darken modeline-bg 0.15))
   (modeline-inactive-fg base6)

   (org-quote `(,(doom-lighten (car bg) 0.05) "#1f1f1f")))

     ;;;; Base theme face overrides
  (;;;; JetBrains editor colors
   (default :background bg :foreground fg) ; TEXT is fg0; gruvbox text is fg1
   (cursor :background fg-hard)
   (hl-line :background base3)             ; CARET_ROW_COLOR
   ((line-number &override) :foreground grey)
   ((line-number-current-line &override) :background base3 :foreground yellow)
   (isearch :foreground base0 :background orange)
   ;; SEARCH_RESULT #5d4d2a; bg3 is also the selection color, but
   ;; the two rarely show at once.
   (lazy-highlight :background base4 :foreground 'unspecified)
   ;; One step above the selection, so both show inside it.
   (show-paren-match :background base4)    ; MATCHED_BRACE #3b514d
   ((show-paren-mismatch &override) :foreground 'unspecified :background error)
      ;;;; JetBrains syntax colors
   (font-lock-comment-face       :foreground comments :slant 'italic)
   ;; The plugin's #83786e is darker than comments; use the same gray.
   (font-lock-doc-face           :foreground comments :slant 'normal)
   (font-lock-function-call-face :foreground functions)
   (font-lock-variable-use-face  :foreground variables)
   (font-lock-property-name-face :foreground variables) ; DEFAULT_INSTANCE_FIELD
   (font-lock-property-use-face  :foreground variables)
   (font-lock-bracket-face       :foreground fg)        ; DEFAULT_BRACES #f8e1aa
   (font-lock-delimiter-face     :foreground fg)        ; DEFAULT_COMMA, _SEMICOLON
   (font-lock-punctuation-face   :foreground fg)        ; DEFAULT_DOT
   (font-lock-escape-face        :foreground dark-yellow)
   (font-lock-preprocessor-face  :foreground teal)      ; DEFAULT_METADATA
      ;;;; eglot
   ;; IDENTIFIER_UNDER_CARET #3a4337.  Its nearest, bg1, is the
   ;; current-line color, and the cursor sits on this symbol; bg2 is
   ;; the selection, so one step above that.
   (eglot-highlight-symbol-face :background base4)
   ;; Module paths read as plain text in JetBrains, not keywords.
   (eglot-semantic-namespace :foreground fg)
   ;; Eglot paints modifier faces over the token type's face, so
   ;; their stock colors (function color on declarations, builtin on
   ;; std items, keyword on statics) win.  JetBrains colors by type
   ;; alone.
   (eglot-semantic-declaration    :inherit nil)
   (eglot-semantic-definition     :inherit nil)
   (eglot-semantic-readonly       :inherit nil)
   (eglot-semantic-static         :inherit nil)
   (eglot-semantic-abstract       :inherit nil)
   (eglot-semantic-async          :inherit nil)
   (eglot-semantic-modification   :inherit nil)
   (eglot-semantic-defaultLibrary :inherit nil)
   ;; Stock eglot paints operators in the function color and
   ;; decorators (Rust attributes) in the type color.
   (eglot-semantic-operator  :foreground operators)
   ;; DEFAULT_METADATA, like the preprocessor.
   (eglot-semantic-decorator :inherit 'font-lock-preprocessor-face)
      ;;;; Rust, from RustRover's Darcula additions (see the top of
      ;;;; gruvbox-jb-themes.el); originals in comments
   (gruvbox-jb-rust-enum         :foreground dark-cyan :slant 'italic) ; 86985d
   (gruvbox-jb-rust-enum-member  :foreground dark-blue :slant 'italic) ; 6da1ab
   (gruvbox-jb-rust-struct       :foreground dark-cyan)                ; 86985d
   (gruvbox-jb-rust-type-alias   :foreground dark-cyan :slant 'italic) ; 86985d
   (gruvbox-jb-rust-trait        :foreground magenta)                  ; 8d91dc
   (gruvbox-jb-rust-macro        :foreground dark-blue)                ; 4eade5
   (gruvbox-jb-rust-type-param   :foreground dark-blue)                ; 20999d
   (gruvbox-jb-rust-lifetime     :foreground dark-blue :slant 'italic) ; 20999d
   (gruvbox-jb-rust-self         :foreground violet)                   ; d98eb3
   (gruvbox-jb-rust-trait-method :foreground yellow :slant 'italic)    ; ffc66d
      ;;;; Python, from the plugin's PY.* colors
   (gruvbox-jb-python-definition       :foreground dark-cyan)   ; PY.FUNC/CLASS_DEFINITION
   (gruvbox-jb-python-decorator        :foreground dark-orange) ; PY.DECORATOR
   (gruvbox-jb-python-self             :foreground blue)        ; PY.SELF_PARAMETER
   (gruvbox-jb-python-keyword-argument :foreground fg)          ; PY.KEYWORD_ARGUMENT
      ;;;; rainbow-delimiters: no red (unmatched parens), gray
      ;;;; (comments) or bright aqua (functions); neighbors differ in
      ;;;; hue.  1-6 are gruvbox's dimmer neutral colors, so parens stay
      ;;;; below the code; 7-9 borrow bright ones to avoid repeats.
   (rainbow-delimiters-depth-1-face :foreground dark-yellow)
   (rainbow-delimiters-depth-2-face :foreground dark-blue)
   (rainbow-delimiters-depth-3-face :foreground dark-green)
   (rainbow-delimiters-depth-4-face :foreground magenta)
   (rainbow-delimiters-depth-5-face :foreground dark-orange)
   (rainbow-delimiters-depth-6-face :foreground dark-cyan)
   (rainbow-delimiters-depth-7-face :foreground yellow)
   (rainbow-delimiters-depth-8-face :foreground blue)
   (rainbow-delimiters-depth-9-face :foreground violet)
      ;;;; consult: file paths borrow the function color by default;
      ;;;; color them as strings
   (consult-file :foreground strings)
      ;;;; blamer
   (blamer-face :foreground yellow :background 'unspecified :slant 'italic)
      ;;;; treemacs: the stock untracked face is a beige close to plain
      ;;;; file names
   (treemacs-git-untracked-face :foreground green)
   (treemacs-git-added-face     :foreground blue)
      ;;;; whitespace: tabs and trailing spaces as quiet gray marks
   (whitespace-tab      :foreground base4 :background 'unspecified)
   ;; doom's base gives these a red background and a red bold
   ;; foreground; drop both to keep syntax colors visible.
   (whitespace-trailing :foreground base4 :background 'unspecified :inherit nil)
   (whitespace-line     :foreground 'unspecified :background base3
                        :weight 'normal :underline t)
   (whitespace-space-before-tab :foreground violet :background base3)

      ;;;; The rest is doom-gruvbox's, minus its evil, helm, ivy, swiper,
      ;;;; neotree, mu4e and vimish-fold faces
   (button :foreground cyan :underline t :weight 'bold)
   ((link &override) :foreground violet)
   (minibuffer-prompt :foreground cyan)
   (mode-line
    :background base3 :foreground modeline-fg ; doom-gruvbox's #37302f
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background bg :foreground base4
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-inactive-bg)))
      ;;;; company
   (company-preview-common :foreground cyan)
   (company-tooltip-common :foreground cyan)
   (company-tooltip-common-selection :foreground cyan)
   (company-tooltip-annotation :foreground cyan)
   (company-tooltip-annotation-selection :foreground cyan)
   (company-scrollbar-bg :background base3)
   (company-scrollbar-fg :background cyan)
   (company-tooltip-selection :background bg-alt2)
   (company-tooltip-mouse :background bg-alt2 :foreground nil)
      ;;;; css-mode
   (css-proprietary-property :foreground keywords)
      ;;;; doom-modeline
   (doom-modeline-project-dir :bold t :foreground cyan)
   (doom-modeline-buffer-path :inherit 'bold :foreground green)
   (doom-modeline-buffer-file :inherit 'bold :foreground fg)
   (doom-modeline-buffer-modified :inherit 'bold :foreground yellow)
   (doom-modeline-error :background bg)
   (doom-modeline-buffer-major-mode :foreground green :bold t)
   (doom-modeline-info :bold t :foreground cyan)
   (doom-modeline-bar :background dark-green)
   (doom-modeline-panel :background dark-green :foreground fg)
      ;;;; emacs-lisp-mode
   (highlight-quoted-symbol :foreground dark-cyan)
      ;;;; ediff
   (ediff-fine-diff-A    :background (doom-blend red bg 0.4) :weight 'bold)
   (ediff-current-diff-A :background (doom-blend red bg 0.2))
      ;;;; flycheck
   (flycheck-error   :underline `(:style wave :color ,red)    :background base3)
   (flycheck-warning :underline `(:style wave :color ,yellow) :background base3)
   (flycheck-info    :underline `(:style wave :color ,blue)   :background base3)
      ;;;; dired
   (dired-directory :foreground cyan)
   (dired-marked :foreground yellow)
   (dired-symlink :foreground cyan)
   (dired-header :foreground cyan)
      ;;;; highlight-thing / highlight-symbol
   (highlight-thing :background (doom-lighten base3 0.03) :distant-foreground fg-alt)
   (highlight-symbol-face :background (doom-lighten base3 0.03) :distant-foreground fg-alt)
      ;;;; LaTeX-mode
   (font-latex-math-face :foreground dark-cyan)
      ;;;; magit
   (magit-section-heading             :foreground cyan :weight 'bold)
   (magit-branch-current              :underline green :inherit 'magit-branch-local)
   (magit-diff-hunk-heading           :background base3 :foreground fg-alt)
   (magit-diff-hunk-heading-highlight :background bg-alt2 :foreground fg)
   (magit-diff-context                :foreground base3 :foreground fg-alt)
      ;;;; markdown-mode
   (markdown-header-face :inherit 'bold :foreground green)
   (markdown-header-delimiter-face :foreground orange)
   (markdown-blockquote-face :inherit 'italic :foreground grey)
   (markdown-list-face :foreground grey)
   (markdown-url-face :foreground violet)
   (markdown-pre-face  :foreground cyan)
   (markdown-link-face :inherit 'underline :foreground grey)
   ((markdown-code-face &override) :background (doom-lighten base2 0.045))
      ;;;; outline
   ((outline-1 &override) :foreground violet)
   ((outline-2 &override) :foreground cyan)
   ((outline-3 &override) :foreground green)
   ((outline-4 &override) :foreground (doom-lighten violet 0.2))
   ((outline-5 &override) :foreground (doom-lighten dark-cyan 0.25))
   ((outline-6 &override) :foreground (doom-lighten violet 0.4))
   ((outline-7 &override) :foreground (doom-lighten dark-cyan 0.5))
   ((outline-8 &override) :foreground (doom-lighten violet 0.6))
      ;;;; org
   ((org-code &override) :foreground orange)
   (org-date :foreground green)
   (org-document-info :foreground red)
   (org-document-title :foreground red)
   (org-drawer :foreground (doom-lighten cyan 0.4))
   (org-ellipsis :underline nil :foreground orange)
   (org-formula :foreground green)
   (org-meta-line :foreground comments)
   (org-list-dt :foreground cyan)
   ((org-quote &override) :inherit 'italic :foreground base7 :background org-quote)
   (org-table :foreground cyan)
   (org-tag :foreground (doom-darken comments 0.15) :weight 'normal)
   (org-todo :foreground green :bold 'inherit)
   (org-verbatim :foreground yellow)
      ;;;; undo-tree
   (undo-tree-visualizer-active-branch-face :foreground cyan)
   (undo-tree-visualizer-current-face :foreground yellow)
      ;;;; web-mode
   (web-mode-html-tag-bracket-face :foreground blue)
   (web-mode-html-tag-face         :foreground cyan)
   (web-mode-html-attr-name-face   :foreground cyan)
   (web-mode-json-key-face         :foreground green)
   (web-mode-json-context-face     :foreground cyan)
      ;;;; which-key
   (which-key-key-face                   :foreground green)
   (which-key-group-description-face     :foreground red)
   (which-key-command-description-face   :foreground blue)
   (which-key-local-map-description-face :foreground orange)))

;;; gruvbox-jb-theme.el ends here
