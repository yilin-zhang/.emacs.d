;;; org-agenda-compose.el --- Compose `org-agenda-files' from multiple sources -*- lexical-binding: t; -*-

;; Author: Yilin Zhang
;; Keywords: convenience, outlines

;;; Commentary:
;;
;; `org-agenda-compose' rebuilds `org-agenda-files' by merging two
;; kinds of input:
;;
;;   1. A static list of files configured by the user via the
;;      `org-agenda-compose-extra-files' defcustom.
;;
;;   2. A set of dynamic source functions registered on the
;;      `org-agenda-compose-functions' hook.  Each function takes no
;;      arguments and returns a list of absolute file paths.  The
;;      typical use case is "all denote notes tagged with _agenda".
;;
;; Both inputs are merged and deduplicated into `org-agenda-files'
;; whenever `org-agenda-compose-refresh' runs.  The package wires the
;; refresh up automatically:
;;
;;   - Once when `org' itself loads (via `with-eval-after-load').
;;   - As a `:before' advice on every `org-agenda' invocation, so
;;     newly-created files (e.g. fresh denote notes) are picked up
;;     without restarting Emacs.
;;
;; `directory-files' is millisecond-range for typical note collections
;; so refreshing on every `org-agenda' call is essentially free.
;;
;; Usage:
;;
;;   (require 'org-agenda-compose)
;;
;;   ;; In your personal config (e.g. custom-post.el):
;;   (setq org-agenda-compose-extra-files
;;         '("~/agenda.org" "~/work.org"))
;;
;;   ;; A dynamic source -- elsewhere in the codebase:
;;   (defun my-denote-agenda-files ()
;;     (require 'denote)
;;     (directory-files denote-directory t "_agenda.*\\.org\\'"))
;;   (add-hook 'org-agenda-compose-functions #'my-denote-agenda-files)
;;
;; A manual refresh is available as `M-x org-agenda-compose-refresh'.

;;; Code:

(defgroup org-agenda-compose nil
  "Compose `org-agenda-files' from static and dynamic sources."
  :group 'org-agenda
  :prefix "org-agenda-compose-")

(defcustom org-agenda-compose-extra-files nil
  "Static list of files to include in `org-agenda-files'.
Each entry is an absolute file path.  This is the user-facing
configuration entry point -- set it in your personal config (e.g.
custom-post.el) to add files that should always appear on the
agenda."
  :type '(repeat file)
  :group 'org-agenda-compose)

(defvar org-agenda-compose-functions nil
  "Functions contributing additional paths to `org-agenda-files'.
Each function takes no arguments and returns a list of absolute
file paths.  Called every time `org-agenda-compose-refresh' runs,
so functions should be cheap (millisecond-range) and must return
freshly-built lists -- the result is mutated downstream.")

;;;###autoload
(defun org-agenda-compose-refresh ()
  "Rebuild `org-agenda-files' from static and dynamic sources.
Combines `org-agenda-compose-extra-files' with everything returned
by functions in `org-agenda-compose-functions', deduplicated.
Safe to call repeatedly."
  (interactive)
  (setq org-agenda-files
        (delete-dups
         (append org-agenda-compose-extra-files
                 (mapcan #'funcall org-agenda-compose-functions)))))

;; Refresh on every `org-agenda' invocation so newly-created
;; dynamic-source files (e.g. fresh denote notes) are picked up
;; without restarting Emacs.  `advice-add' attaches to the symbol,
;; so this works even if `org-agenda' has not been defined yet.
(advice-add 'org-agenda :before
            (lambda (&rest _)
              (org-agenda-compose-refresh)))

;; Initial population once `org' has loaded.  Without this, code
;; that reads `org-agenda-files' before the first `org-agenda' call
;; would see an empty list.
(with-eval-after-load 'org
  (org-agenda-compose-refresh))

(provide 'org-agenda-compose)
;;; org-agenda-compose.el ends here
