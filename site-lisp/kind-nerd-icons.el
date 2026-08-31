;;; kind-nerd-icons.el  -*- lexical-binding: t; -*-

(require 'nerd-icons)
(require 'seq)
(require 'subr-x)

(defgroup kind-nerd-icons nil
  "Nerd Font icons for completion and symbol kinds."
  :group 'convenience)

(defvar kind-nerd-icons--cache nil
  "The cache of styled and padded label (text or icon).
An alist.")

(defun kind-nerd-icons-reset-cache ()
  "Remove all cached icons from `kind-nerd-icons--cache'."
  (interactive)
  (setq kind-nerd-icons--cache nil))

(defun kind-nerd-icons--set-default-clear-cache (&rest args)
  (kind-nerd-icons-reset-cache)
  (apply #'set-default args))

(defconst kind-nerd-icons--icons
  `(;; The kinds LSP defines, in the order `CompletionItemKind' lists
    ;; them.  Every language server reports from this set and no other.
    (text . ,(nerd-icons-codicon "nf-cod-text_size" :face 'font-lock-doc-face))
    (method . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'font-lock-function-name-face))
    (function . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'font-lock-function-name-face))
    (constructor . ,(nerd-icons-codicon "nf-cod-triangle_right" :face 'font-lock-function-name-face))
    (field . ,(nerd-icons-codicon "nf-cod-symbol_field" :face 'font-lock-variable-name-face))
    (variable . ,(nerd-icons-codicon "nf-cod-symbol_variable" :face 'font-lock-variable-name-face))
    (class . ,(nerd-icons-codicon "nf-cod-symbol_class" :face 'font-lock-type-face))
    (interface . ,(nerd-icons-codicon "nf-cod-symbol_interface" :face 'font-lock-type-face))
    (module . ,(nerd-icons-codicon "nf-cod-file_submodule" :face 'font-lock-preprocessor-face))
    (property . ,(nerd-icons-codicon "nf-cod-symbol_property" :face 'font-lock-variable-name-face))
    (unit . ,(nerd-icons-codicon "nf-cod-symbol_ruler" :face 'font-lock-constant-face))
    (value . ,(nerd-icons-codicon "nf-cod-symbol_field" :face 'font-lock-builtin-face))
    (enum . ,(nerd-icons-codicon "nf-cod-symbol_enum" :face 'font-lock-builtin-face))
    (keyword . ,(nerd-icons-codicon "nf-cod-symbol_keyword" :face 'font-lock-keyword-face))
    (snippet . ,(nerd-icons-codicon "nf-cod-symbol_snippet" :face 'font-lock-string-face))
    (color . ,(nerd-icons-codicon "nf-cod-symbol_color" :face 'success))
    (file . ,(nerd-icons-codicon "nf-cod-symbol_file" :face 'font-lock-string-face))
    (reference . ,(nerd-icons-codicon "nf-cod-references" :face 'font-lock-variable-name-face))
    (folder . ,(nerd-icons-codicon "nf-cod-folder" :face 'font-lock-doc-face))
    (enummember . ,(nerd-icons-codicon "nf-cod-symbol_enum_member" :face 'font-lock-builtin-face))
    (constant . ,(nerd-icons-codicon "nf-cod-symbol_constant" :face 'font-lock-constant-face))
    (struct . ,(nerd-icons-codicon "nf-cod-symbol_structure" :face 'font-lock-variable-name-face))
    (event . ,(nerd-icons-codicon "nf-cod-symbol_event" :face 'font-lock-warning-face))
    (operator . ,(nerd-icons-codicon "nf-cod-symbol_operator" :face 'font-lock-comment-delimiter-face))
    (typeparameter . ,(nerd-icons-codicon "nf-cod-list_unordered" :face 'font-lock-type-face))
    ;; The same two kinds under the hyphenated names some callers use.
    (enum-member . ,(nerd-icons-codicon "nf-cod-symbol_enum_member" :face 'font-lock-builtin-face))
    (type-parameter . ,(nerd-icons-codicon "nf-cod-list_unordered" :face 'font-lock-type-face))
    ;; Kinds outside the specification.  Individual backends invent
    ;; these -- `command' and `macro' come from Emacs Lisp, `magic' and
    ;; `param' from Python servers, `array', `boolean', `numeric' and
    ;; `string' from JSON-shaped data -- and `package' is ours, for
    ;; imenu headings that no LSP kind covers.
    (array . ,(nerd-icons-codicon "nf-cod-symbol_array" :face 'font-lock-type-face))
    (boolean . ,(nerd-icons-codicon "nf-cod-symbol_boolean" :face 'font-lock-builtin-face))
    (command . ,(nerd-icons-codicon "nf-cod-terminal" :face 'default))
    (macro . ,(nerd-icons-codicon "nf-cod-symbol_misc" :face 'font-lock-keyword-face))
    (magic . ,(nerd-icons-codicon "nf-cod-wand" :face 'font-lock-builtin-face))
    (numeric . ,(nerd-icons-codicon "nf-cod-symbol_numeric" :face 'font-lock-builtin-face))
    (package . ,(nerd-icons-codicon "nf-cod-package" :face 'font-lock-preprocessor-face))
    (param . ,(nerd-icons-codicon "nf-cod-symbol_parameter" :face 'default))
    (string . ,(nerd-icons-codicon "nf-cod-symbol_string" :face 'font-lock-string-face))
    ;; Anything unlisted.
    (t . ,(nerd-icons-codicon "nf-cod-code" :face 'font-lock-warning-face))))

(defun kind-nerd-icons-icon (kind)
  "Return the icon for symbol kind KIND, or nil when the table has none.
An unknown kind gets nothing rather than the fallback icon: a caller
that decorates a whole column wants the column to stay informative."
  (and kind (cdr (assq kind kind-nerd-icons--icons))))

(defcustom kind-nerd-icons-label-aliases
  '((type . class)
    (section . nil))
  "Imenu headings whose name is not the kind that fits them.
The car is the singular, downcased heading; the cdr is a key into
`kind-nerd-icons--icons', or nil for no icon at all.  A heading whose
name already is a kind -- \"Packages\", \"Variables\" -- needs no entry."
  :type '(alist :key-type symbol
                :value-type (choice (symbol :tag "Kind")
                                    (const :tag "No icon" nil)))
  :group 'kind-nerd-icons)

(defun kind-nerd-icons-from-label (label)
  "Return the symbol kind the imenu heading LABEL stands for, or nil.
Headings are plural and kinds are singular, and which letters to drop is
not decidable from the ending alone -- \"variables\" ends in \"es\" as
surely as \"classes\" does -- so every candidate is tried in turn.

This is for indexes that label nothing themselves and only group symbols
under headings.  A language server states each symbol\='s kind outright,
and that always wins over a guess made from a heading."
  (let* ((name (downcase (string-trim label)))
         (candidates
          (delq nil (list name
                          (and (string-suffix-p "s" name) (substring name 0 -1))
                          (and (string-suffix-p "es" name) (substring name 0 -2)))))
         ;; Wrapped in a list so that an alias to nil still counts as a
         ;; hit and stops the search.
         (hit (seq-some
               (lambda (n)
                 (let ((sym (intern n)))
                   (cond ((assq sym kind-nerd-icons-label-aliases)
                          (list (cdr (assq sym kind-nerd-icons-label-aliases))))
                         ((assq sym kind-nerd-icons--icons) (list sym)))))
               candidates)))
    (car hit)))

(defsubst kind-nerd-icons--metadata-get (metadata type-name)
  "Get METADATA for keyword TYPE-NAME from the completion properties."
  (or
   (plist-get completion-extra-properties (intern (format ":%s" type-name)))
   (cdr (assq (intern type-name) metadata))))

(defun kind-nerd-icons-formatted (kind)
  "Get icon for KIND."
  (or (alist-get kind kind-nerd-icons--cache)
      (let ((map (assq kind kind-nerd-icons--icons)))
        (let*  ((icon (if map
                          (cdr map)
                        (cdr (assq t kind-nerd-icons--icons))))
                (half (/ (default-font-width) 2))
                (pad (propertize " " 'display `(space :width (,half))))
                (disp (concat pad icon pad)))
          (setf (alist-get kind kind-nerd-icons--cache) disp)
          disp))))

(defun kind-nerd-icons-margin-formatter (metadata)
  "Return a margin-formatter function which produces kind icons.
METADATA is the completion metadata supplied by the caller (see
info node `(elisp)Programmed Completion').  To use, add this
function to the relevant margin-formatters list."
  (if-let* ((kind-func (kind-nerd-icons--metadata-get metadata "company-kind")))
      (lambda (cand)
        (if-let* ((kind (funcall kind-func cand)))
            (kind-nerd-icons-formatted kind)
          (kind-nerd-icons-formatted t))))) ;; as a backup


(provide 'kind-nerd-icons)
;;; kind-nerd-icons.el ends here
