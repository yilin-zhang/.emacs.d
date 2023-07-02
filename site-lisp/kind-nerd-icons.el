;;; kind-nerd-icons.el  -*- lexical-binding: t; -*-

(require 'nerd-icons)

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
  `((array . ,(nerd-icons-codicon "nf-cod-symbol_array" :face 'font-lock-type-face))
    (boolean . ,(nerd-icons-codicon "nf-cod-symbol_boolean" :face 'font-lock-builtin-face))
    (class . ,(nerd-icons-codicon "nf-cod-symbol_class" :face 'font-lock-type-face))
    (color . ,(nerd-icons-codicon "nf-cod-symbol_color" :face 'success) )
    (command . ,(nerd-icons-codicon "nf-cod-terminal" :face 'default) )
    (constant . ,(nerd-icons-codicon "nf-cod-symbol_constant" :face 'font-lock-constant-face) )
    (constructor . ,(nerd-icons-codicon "nf-cod-triangle_right" :face 'font-lock-function-name-face) )
    (enummember . ,(nerd-icons-codicon "nf-cod-symbol_enum_member" :face 'font-lock-builtin-face) )
    (enum-member . ,(nerd-icons-codicon "nf-cod-symbol_enum_member" :face 'font-lock-builtin-face) )
    (enum . ,(nerd-icons-codicon "nf-cod-symbol_enum" :face 'font-lock-builtin-face) )
    (event . ,(nerd-icons-codicon "nf-cod-symbol_event" :face 'font-lock-warning-face) )
    (field . ,(nerd-icons-codicon "nf-cod-symbol_field" :face 'font-lock-variable-name-face) )
    (file . ,(nerd-icons-codicon "nf-cod-symbol_file" :face 'font-lock-string-face) )
    (folder . ,(nerd-icons-codicon "nf-cod-folder" :face 'font-lock-doc-face) )
    (interface . ,(nerd-icons-codicon "nf-cod-symbol_interface" :face 'font-lock-type-face) )
    (keyword . ,(nerd-icons-codicon "nf-cod-symbol_keyword" :face 'font-lock-keyword-face) )
    (macro . ,(nerd-icons-codicon "nf-cod-symbol_misc" :face 'font-lock-keyword-face) )
    (magic . ,(nerd-icons-codicon "nf-cod-wand" :face 'font-lock-builtin-face) )
    (method . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'font-lock-function-name-face) )
    (function . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'font-lock-function-name-face) )
    (module . ,(nerd-icons-codicon "nf-cod-file_submodule" :face 'font-lock-preprocessor-face) )
    (numeric . ,(nerd-icons-codicon "nf-cod-symbol_numeric" :face 'font-lock-builtin-face) )
    (operator . ,(nerd-icons-codicon "nf-cod-symbol_operator" :face 'font-lock-comment-delimiter-face) )
    (param . ,(nerd-icons-codicon "nf-cod-symbol_parameter" :face 'default) )
    (property . ,(nerd-icons-codicon "nf-cod-symbol_property" :face 'font-lock-variable-name-face) )
    (reference . ,(nerd-icons-codicon "nf-cod-references" :face 'font-lock-variable-name-face) )
    (snippet . ,(nerd-icons-codicon "nf-cod-symbol_snippet" :face 'font-lock-string-face) )
    (string . ,(nerd-icons-codicon "nf-cod-symbol_string" :face 'font-lock-string-face) )
    (struct . ,(nerd-icons-codicon "nf-cod-symbol_structure" :face 'font-lock-variable-name-face) )
    (text . ,(nerd-icons-codicon "nf-cod-text_size" :face 'font-lock-doc-face) )
    (typeparameter . ,(nerd-icons-codicon "nf-cod-list_unordered" :face 'font-lock-type-face) )
    (type-parameter . ,(nerd-icons-codicon "nf-cod-list_unordered" :face 'font-lock-type-face) )
    (unit . ,(nerd-icons-codicon "nf-cod-symbol_ruler" :face 'font-lock-constant-face) )
    (value . ,(nerd-icons-codicon "nf-cod-symbol_field" :face 'font-lock-builtin-face) )
    (variable . ,(nerd-icons-codicon "nf-cod-symbol_variable" :face 'font-lock-variable-name-face) )
    (t . ,(nerd-icons-codicon "nf-cod-code" :face 'font-lock-warning-face))))

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
  (if-let ((kind-func (kind-nerd-icons--metadata-get metadata "company-kind")))
      (lambda (cand)
        (if-let ((kind (funcall kind-func cand)))
            (kind-nerd-icons-formatted kind)
          (kind-nerd-icons-formatted t))))) ;; as a backup


(provide 'kind-nerd-icons)
;;; kind-nerd-icons.el ends here
