;;; breadcrumb-nerd-icons.el --- Nerd Font icons for breadcrumb  -*- lexical-binding: t; -*-

;;; Commentary:

;; Puts an icon in front of the breadcrumb crumbs that stand for
;; something identifiable: a file icon on the file, and a symbol-kind
;; icon -- function, class, variable -- on the imenu nodes.  The
;; directories leading to the file are left bare on purpose: a row of
;; identical folder icons says nothing the separators do not already say.
;;
;; Icons come from `kind-nerd-icons--icons', the same table corfu's
;; margin formatter uses, so a symbol looks the same in the header line
;; as it does in the completion popup.
;;
;; Kinds come from the language server, which labels every node it
;; reports.  A plain imenu index carries no such labels, so two rules
;; fill the gap: a node with no label of its own takes the kind of the
;; imenu heading above it ("Variables", "Packages"), and a lone
;; top-level node in a Lisp buffer is a function -- that is precisely
;; what the nil menu titles in `lisp-imenu-generic-expression' mean.
;;
;; Breadcrumb draws plain text and offers no hook for decorating a
;; crumb, so all of this is installed as advice.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'nerd-icons)
(require 'kind-nerd-icons)
(require 'breadcrumb)

(defgroup breadcrumb-nerd-icons nil
  "Nerd Font icons for `breadcrumb' crumbs."
  :group 'breadcrumb)

(defcustom breadcrumb-nerd-icons-kind-aliases
  '((type . class)
    (section . nil))
  "Imenu headings whose name is not the kind that fits them.
The car is the singular, downcased heading; the cdr is a key into
`kind-nerd-icons--icons', or nil for no icon at all.  A heading whose
name already is a kind -- \"Packages\", \"Variables\" -- needs no entry."
  :type '(alist :key-type symbol
                :value-type (choice (symbol :tag "Kind")
                                    (const :tag "No icon" nil)))
  :group 'breadcrumb-nerd-icons)

(defun breadcrumb-nerd-icons--icon (kind)
  "Return the icon for symbol kind KIND, or nil when there is none.
KIND is a key into `kind-nerd-icons--icons'.  An unknown kind, and a nil
KIND, get no icon rather than a generic one: a column of identical
glyphs carries no information."
  (and kind (cdr (assq kind kind-nerd-icons--icons))))

(defun breadcrumb-nerd-icons--kind-from-label (label)
  "Return the symbol kind the imenu heading LABEL stands for, or nil.
Headings are plural and kinds are singular, and which letters to drop is
not decidable from the ending alone -- \"variables\" ends in \"es\" as
surely as \"classes\" does -- so every candidate is tried in turn."
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
                   (cond ((assq sym breadcrumb-nerd-icons-kind-aliases)
                          (list (cdr (assq sym breadcrumb-nerd-icons-kind-aliases))))
                         ((assq sym kind-nerd-icons--icons) (list sym)))))
               candidates)))
    (car hit)))

(defun breadcrumb-nerd-icons--resolve-kinds (ipath)
  "Tag every node of IPATH with the symbol kind it stands for.
Filter-return advice for `breadcrumb-ipath'.  The kind is stored under
`breadcrumb-nerd-icons-kind' on the node's first character."
  (let ((lispp (derived-mode-p 'lisp-data-mode))
        (lone (= (length ipath) 1))
        inherited)
    (mapcar
     (lambda (p)
       (let* ((own (get-text-property 0 'breadcrumb-kind p))
              (kind (or (and own (intern (downcase own)))
                        (breadcrumb-nerd-icons--kind-from-label
                         (substring-no-properties p))
                        inherited
                        (and lispp lone 'function))))
         (setq inherited kind)
         (if (null kind)
             p
           (let ((p (copy-sequence p)))
             (put-text-property 0 1 'breadcrumb-nerd-icons-kind kind p)
             p))))
     ipath)))

(defun breadcrumb-nerd-icons--decorate (icon crumb)
  "Return CRUMB with ICON in front of it.
The icon carries the properties breadcrumb left on the crumb -- the
mouse ones and `breadcrumb-dont-shorten' -- so icon and name stay a
single uniform run.  Otherwise they highlight and click as two separate
regions, and the leaf loses its exemption from shortening.

A crumb that `breadcrumb--summarize' shortens to one character is left
as just its icon.  Only interior nodes are ever shortened, and only when
the window is too narrow to hold them."
  (if (or (null icon) (string-empty-p crumb))
      crumb
    (let ((prefix (concat icon " ")))
      (add-text-properties
       0 (length prefix)
       (list 'breadcrumb-dont-shorten
             (get-text-property 0 'breadcrumb-dont-shorten crumb)
             'mouse-face (get-text-property 0 'mouse-face crumb)
             'help-echo (get-text-property 0 'help-echo crumb)
             'keymap (get-text-property 0 'keymap crumb))
       prefix)
      (concat prefix crumb))))

(defun breadcrumb-nerd-icons--imenu-node (crumb)
  "Prefix CRUMB, an imenu node, with an icon for its symbol kind.
Filter-return advice for `breadcrumb--format-ipath-node'."
  (breadcrumb-nerd-icons--decorate
   (breadcrumb-nerd-icons--icon
    (get-text-property 0 'breadcrumb-nerd-icons-kind crumb))
   crumb))

(defun breadcrumb-nerd-icons--project-node (fn p more &rest args)
  "Give the file crumb a file icon, leaving the directories bare.
Around advice for `breadcrumb--format-project-node', which see for FN,
P, MORE and ARGS.  MORE is nil only on the last node, the file itself."
  (let ((crumb (apply fn p more args)))
    (if more
        crumb
      (breadcrumb-nerd-icons--decorate
       (nerd-icons-icon-for-file (substring-no-properties crumb))
       crumb))))

(defun breadcrumb-nerd-icons--reindex ()
  "Drop the imenu index breadcrumb built before the language server attached.
At startup breadcrumb indexes the buffer with the major mode's own
imenu, whose nodes carry no `breadcrumb-kind', and it reindexes only
once the buffer is modified -- so without this the symbol icons stay
missing until the first edit."
  (when (bound-and-true-p breadcrumb-local-mode)
    (setq imenu--index-alist nil
          breadcrumb--ipath-plain-cache nil
          breadcrumb--last-update-tick -1)))

;;;###autoload
(define-minor-mode breadcrumb-nerd-icons-mode
  "Show Nerd Font icons in `breadcrumb' crumbs."
  :init-value nil
  :global t
  :group 'breadcrumb-nerd-icons
  (if breadcrumb-nerd-icons-mode
      (progn
        (advice-add 'breadcrumb-ipath :filter-return
                    #'breadcrumb-nerd-icons--resolve-kinds)
        (advice-add 'breadcrumb--format-ipath-node :filter-return
                    #'breadcrumb-nerd-icons--imenu-node)
        (advice-add 'breadcrumb--format-project-node :around
                    #'breadcrumb-nerd-icons--project-node)
        (add-hook 'eglot-managed-mode-hook #'breadcrumb-nerd-icons--reindex))
    (advice-remove 'breadcrumb-ipath #'breadcrumb-nerd-icons--resolve-kinds)
    (advice-remove 'breadcrumb--format-ipath-node
                   #'breadcrumb-nerd-icons--imenu-node)
    (advice-remove 'breadcrumb--format-project-node
                   #'breadcrumb-nerd-icons--project-node)
    (remove-hook 'eglot-managed-mode-hook #'breadcrumb-nerd-icons--reindex)))

(provide 'breadcrumb-nerd-icons)
;;; breadcrumb-nerd-icons.el ends here
