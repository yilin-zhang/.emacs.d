;;; sailor.el --- Personal journaling minor mode -*- lexical-binding: t; -*-

;; Author: Yilin Zhang
;; Keywords: convenience, outlines

;;; Commentary:
;;
;; Sailor is a minor mode for personal journaling in Org files.  It
;; replaces `hl-todo-keyword-faces' buffer-locally with a
;; journal-specific keyword set, so journal buffers highlight entries
;; like DINNER, THOUGHT, or BACKLOG without polluting the global
;; `hl-todo-keyword-faces' that programming buffers rely on.
;;
;; The journal keyword set is intentionally a FULL replacement of the
;; global one, not an extension: a buffer running `sailor-mode' shows
;; only journal keywords, not the programming defaults.
;;
;; Usage:
;;
;;   Add a file-local variable to the top of your journal file:
;;
;;     # -*- sailor-enable: t; -*-
;;     #+TITLE: My Journal
;;     ...
;;
;;   Reopen the file; `sailor-mode' turns on automatically via
;;   `hack-local-variables-hook'.  `sailor-enable' is registered as a
;;   safe file-local variable, so Emacs won't prompt for confirmation.
;;
;;   To change the keyword set or colors, customize
;;   `sailor-keyword-faces'.

;;; Code:

(require 'hl-todo)
(require 'ansi-color)

(defgroup sailor nil
  "Personal journaling minor mode."
  :group 'convenience
  :prefix "sailor-")

(defcustom sailor-keyword-faces
  `(;; work
    ("WORK"      . ,(face-foreground 'ansi-color-bright-black))
    ("MEET"      . ,(face-foreground 'ansi-color-bright-black))
    ;; thoughts
    ("IDEA"      . ,(face-foreground 'ansi-color-blue))
    ("THOUGHT"   . ,(face-foreground 'ansi-color-blue))
    ("FEEL"      . ,(face-foreground 'ansi-color-blue))
    ;; daily activities
    ("DINNER"    . ,(face-foreground 'ansi-color-white))
    ("LUNCH"     . ,(face-foreground 'ansi-color-white))
    ("MORNING"   . ,(face-foreground 'ansi-color-white))
    ("AFTERNOON" . ,(face-foreground 'ansi-color-white))
    ("EVENING"   . ,(face-foreground 'ansi-color-white))
    ;; actions
    ("BOUGHT"    . ,(face-foreground 'ansi-color-bright-magenta))
    ("SUBS"      . ,(face-foreground 'ansi-color-bright-magenta))
    ("CALL"      . ,(face-foreground 'ansi-color-bright-magenta))
    ;; reading, watching, gaming
    ("READ"      . ,(face-foreground 'ansi-color-bright-green))
    ("PLAYED"    . ,(face-foreground 'ansi-color-bright-green))
    ("WATCHED"   . ,(face-foreground 'ansi-color-bright-green))
    ("FINISHED"  . ,(face-foreground 'ansi-color-bright-green))
    ;; backlog
    ("BACKLOG"   . ,(face-foreground 'ansi-color-yellow)))
  "Keyword face alist used by `sailor-mode'.
Same shape as `hl-todo-keyword-faces': an alist mapping a keyword
string to a color string or face.  When `sailor-mode' is on, this
alist completely replaces `hl-todo-keyword-faces' buffer-locally
-- it is NOT merged with the global set."
  :type '(alist :key-type string :value-type (choice string face))
  :group 'sailor)

;;;###autoload
(define-minor-mode sailor-mode
  "Minor mode for personal journal entries.

When enabled, replaces `hl-todo-keyword-faces' buffer-locally with
`sailor-keyword-faces', giving the buffer its own dedicated set of
highlighted keywords that do not interfere with the global hl-todo
configuration used in programming buffers.

`hl-todo' normally skips `org-mode' via `hl-todo-exclude-modes',
but this mode invokes `hl-todo-mode' directly, bypassing that
exclusion -- so enabling Sailor in a .org buffer just works."
  :lighter " Sailor"
  (cond
   (sailor-mode
    ;; Tear down any hl-todo state built from the global alist,
    ;; install the journal alist buffer-locally, then re-enable
    ;; hl-todo so font-lock rebuilds its regexp from the new list.
    (when (bound-and-true-p hl-todo-mode)
      (hl-todo-mode -1))
    (setq-local hl-todo-keyword-faces sailor-keyword-faces)
    (hl-todo-mode 1))
   (t
    ;; Restore the global alist in this buffer and rebuild hl-todo.
    (when (bound-and-true-p hl-todo-mode)
      (hl-todo-mode -1))
    (kill-local-variable 'hl-todo-keyword-faces)
    (hl-todo-mode 1))))


;; --- File-local activation ---------------------------------------
;;
;; Journal files opt in to `sailor-mode' by setting the file-local
;; variable `sailor-enable' to t:
;;
;;   # -*- sailor-enable: t; -*-
;;
;; `hack-local-variables-hook' fires after Emacs has processed the
;; file-local block, at which point `sailor--maybe-enable' checks the
;; flag and turns `sailor-mode' on.

(defvar sailor-enable nil
  "If non-nil as a file-local variable, enable `sailor-mode' in the buffer.

This is intentionally separate from the `sailor-mode' variable so
that a simple `booleanp' predicate can mark it safe for use as a
file-local variable, avoiding the unsafe-variable confirmation
prompt.")

(put 'sailor-enable 'safe-local-variable #'booleanp)

(defun sailor--maybe-enable ()
  "Enable `sailor-mode' if `sailor-enable' was set via file-local vars."
  (when sailor-enable
    (sailor-mode 1)))

(add-hook 'hack-local-variables-hook #'sailor--maybe-enable)

(provide 'sailor)
;;; sailor.el ends here
