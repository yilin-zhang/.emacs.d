;;; kind-nerd-icons.el  -*- lexical-binding: t; -*-

(require 'nerd-icons)

(defvar kind-nerd-icons--cache nil
  "The cache of styled and padded label (text or icon).
An alist.")

(defun kind-nerd-icons-reset-cache ()
  "Remove all cached icons from `kind-nerd-icons-mapping'."
  (interactive)
  (setq kind-nerd-icons--cache nil))

(defun kind-nerd-icons--set-default-clear-cache (&rest args)
  (kind-nerd-icons-reset-cache)
  (apply #'set-default args))

(defvar kind-nerd-icons--icons
  `((unknown . ,(nerd-icons-faicon "nf-fa-question"))
    (text . ,(nerd-icons-faicon "nf-fa-text_width"))
    (method . ,(nerd-icons-faicon "nf-fa-cube" :face 'nerd-icons-purple))
    (function . ,(nerd-icons-faicon "nf-fa-cube" :face 'nerd-icons-purple))
    (fun . ,(nerd-icons-faicon "nf-fa-cube" :face 'nerd-icons-purple))
    (constructor . ,(nerd-icons-faicon "nf-fa-cube" :face 'nerd-icons-purple))
    (ctor . ,(nerd-icons-faicon "nf-fa-cube" :face 'nerd-icons-purple))
    (field . ,(nerd-icons-faicon "nf-fa-tag" :face 'nerd-icons-cyan))
    (variable . ,(nerd-icons-faicon "nf-fa-tag" :face 'nerd-icons-cyan))
    (var . ,(nerd-icons-faicon "nf-fa-tag" :face 'nerd-icons-cyan))
    (class . ,(nerd-icons-faicon "nf-fa-cubes" :face 'nerd-icons-orange))
    (interface . ,(nerd-icons-faicon "nf-fa-share_alt" :face 'nerd-icons-orange))
    (i/f . ,(nerd-icons-faicon "nf-fa-share_alt" :face 'nerd-icons-orange))
    (module . ,(nerd-icons-faicon "nf-fa-sitemap" :face 'nerd-icons-orange))
    (mod . ,(nerd-icons-faicon "nf-fa-sitemap" :face 'nerd-icons-orange))
    (property . ,(nerd-icons-faicon "nf-fa-wrench"))
    (prop . ,(nerd-icons-faicon "nf-fa-wrench"))
    (unit . ,(nerd-icons-faicon "nf-fa-gear"))
    (value . ,(nerd-icons-faicon "nf-fa-align_right"))
    (enum . ,(nerd-icons-faicon "nf-fa-th_list" :face 'nerd-icons-orange))
    (keyword . ,(nerd-icons-faicon "nf-fa-key"))
    (k/w . ,(nerd-icons-faicon "nf-fa-key"))
    (snippet . ,(nerd-icons-faicon "nf-fa-align_center"))
    (sn . ,(nerd-icons-faicon "nf-fa-align_center"))
    (color . ,(nerd-icons-faicon "nf-fae-palette_color"))
    (file . ,(nerd-icons-faicon "nf-fa-file"))
    (reference . ,(nerd-icons-faicon "nf-fa-bookmark"))
    (ref . ,(nerd-icons-faicon "nf-fa-bookmark"))
    (folder . ,(nerd-icons-faicon "nf-fa-folder_open"))
    (dir . ,(nerd-icons-faicon "nf-fa-folder_open"))
    (enum-member . ,(nerd-icons-faicon "nf-fa-tag"))
    (enummember . ,(nerd-icons-faicon "nf-fa-tag"))
    (member . ,(nerd-icons-faicon "nf-fa-align_right"))
    (constant . ,(nerd-icons-faicon "nf-fa-square_o"))
    (const . ,(nerd-icons-faicon "nf-fa-square_o"))
    (struct . ,(nerd-icons-faicon "nf-fa-gears" :face 'nerd-icons-orange))
    (event . ,(nerd-icons-faicon "nf-fa-exclamation_circle" :face 'nerd-icons-orange))
    (operator . ,(nerd-icons-faicon "nf-fa-calculator"))
    (op . ,(nerd-icons-faicon "nf-fa-calculator"))
    (type-parameter . ,(nerd-icons-faicon "nf-fa-sliders"))
    (param . ,(nerd-icons-faicon "nf-fa-sliders"))
    (template . ,(nerd-icons-faicon "nf-fa-align_left"))
    (t . ,(nerd-icons-faicon "nf-fa-book"))))

(defsubst kind-nerd-icons--metadata-get (metadata type-name)
  (or
   (plist-get completion-extra-properties (intern (format ":%s" type-name)))
   (cdr (assq (intern type-name) metadata))))

(defun kind-nerd-icons-formatted (kind)
  "Format icon kind with nerd-icons"
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
