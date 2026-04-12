;;; warmup.el --- Idle-time incremental package loading -*- lexical-binding: t; -*-

;; Author: Yilin Zhang
;; Keywords: convenience, internal

;;; Commentary:
;;
;; Warms up heavy packages piece by piece during idle time after
;; Emacs finishes startup, so the first interactive use of a slow
;; command (e.g. `C-c a' for `org-agenda') doesn't pause for hundreds
;; of milliseconds while a tower of sub-packages is required.
;;
;; The classic target is Org -- it's structured as ~17 sub-packages
;; that all load synchronously on first use.  Warming them in small
;; chunks during idle time amortizes the cost invisibly.
;;
;; Adapted from doomemacs's `doom-load-packages-incrementally'.
;;
;; Usage:
;;
;;   ;; Once, in your global startup config:
;;   (use-package warmup
;;     :ensure nil
;;     :load-path "path/to/site-lisp"
;;     :demand t)
;;
;;   ;; Then, in each consumer module:
;;   (use-package org
;;     :warmup (calendar find-func format-spec
;;              org-macs org-compat org-faces org-entities
;;              org-list org-pcomplete org-src org-footnote
;;              org-macro ob org org-clock org-agenda org-capture)
;;     ...)
;;
;; The `:warmup' use-package keyword is registered when this file
;; loads.  Each consumer co-locates its own list with its
;; use-package form, so there's no central registry to maintain.
;;
;; Loading this library is enough to enable warmup at startup -- a
;; top-level `add-hook' attaches `warmup-start' to
;; `emacs-startup-hook'.  No mode toggle, no extra setup call.
;;
;; Order matters: list sub-packages and dependencies before parents.
;; The order across `:warmup' declarations follows init.el's
;; `require' order naturally.
;;
;; The queue is consumed at most once per Emacs session, starting
;; `warmup-first-idle' seconds after `emacs-startup-hook' fires and
;; stepping through one entry every `warmup-idle-step' seconds of
;; idle time.
;;
;; User input always wins: each `require' is wrapped in
;; `while-no-input', so a key press mid-load aborts cleanly and the
;; same package is retried on the next idle tick.

;;; Code:

(defgroup warmup nil
  "Idle-time incremental package warming."
  :group 'convenience
  :prefix "warmup-")

(defcustom warmup-packages nil
  "Packages to require during idle time after startup.
Each entry is a feature symbol passed to `require'.  Order
matters: sub-packages and shared dependencies should come before
the parent package.  Most consumers should use the `:warmup'
use-package keyword rather than mutating this variable directly."
  :type '(repeat symbol)
  :group 'warmup)

(defcustom warmup-first-idle 2.0
  "Seconds of idle after `emacs-startup-hook' before warmup begins.
A small delay lets Emacs fully settle (window manager redraws,
font setup, etc.) before we start consuming idle CPU."
  :type 'number
  :group 'warmup)

(defcustom warmup-idle-step 0.75
  "Seconds of idle required between successive warmup ticks."
  :type 'number
  :group 'warmup)

(defvar warmup--queue nil
  "Internal mutable working copy of `warmup-packages'.
Populated by `warmup-start' and consumed by `warmup--load-next'.
Kept separate so the user-facing `warmup-packages' defcustom
stays pristine and can be inspected after warmup completes.")

(defun warmup--load-next ()
  "Load the next entry from `warmup--queue'.
Wraps `require' in `while-no-input' so user input cancels
mid-load without consuming the entry; that entry will be retried
on the next idle tick.  Suppresses GC and the
file-name-handler-alist during the load to mirror the
startup-time optimizations."
  (when warmup--queue
    (let ((gc-cons-threshold most-positive-fixnum)
          (req (car warmup--queue)))
      (cond
       ;; Already loaded by some other code path -- skip and continue.
       ((featurep req)
        (setq warmup--queue (cdr warmup--queue)))
       ;; Try to load.  `while-no-input' returns nil if interrupted
       ;; by user input; in that case we leave the entry on the
       ;; queue and retry on the next tick.
       (t
        (condition-case err
            (when (while-no-input
                    (let ((inhibit-message t)
                          (file-name-handler-alist
                           (list (rassq 'jka-compr-handler
                                        file-name-handler-alist))))
                      (require req nil t)
                      t))
              (setq warmup--queue (cdr warmup--queue)))
          (error
           (message "warmup: failed to load %S: %S" req err)
           ;; Drop the failing entry so we don't loop forever.
           (setq warmup--queue (cdr warmup--queue))))))
      ;; Schedule another tick if there's still work.
      (when warmup--queue
        (run-with-idle-timer warmup-idle-step nil #'warmup--load-next)))))

;;;###autoload
(defun warmup-add (features)
  "Append FEATURES to `warmup-packages'.
FEATURES is a list of symbols to be `require'd during idle time
after Emacs finishes startup, in the order added.  Safe to call
multiple times from different modules; the registration order
across calls is preserved.

Most consumers should use the `:warmup' use-package keyword
instead of calling this directly.  Both end up in the same queue.

This MUST be called before `emacs-startup-hook' fires (i.e.
during init), since the queue snapshot is taken when
`warmup-start' runs on that hook."
  (setq warmup-packages (append warmup-packages features)))

;;;###autoload
(defun warmup-start ()
  "Begin warming up packages from `warmup-packages' on idle.
Copies the configured list into the internal queue and schedules
the first tick after `warmup-first-idle' seconds of idle time.

Installed automatically on `emacs-startup-hook' when this library
is loaded."
  (interactive)
  (setq warmup--queue (copy-sequence warmup-packages))
  (when warmup--queue
    (run-with-idle-timer warmup-first-idle nil #'warmup--load-next)))

;; Self-install: loading this library is enough to make warmup
;; happen at startup.  No mode toggle, no extra setup call.
;; `add-hook' is idempotent, so re-loading the file is safe.
(add-hook 'emacs-startup-hook #'warmup-start)


;; --- use-package `:warmup' keyword -------------------------------
;;
;; Lets consumers declare warmup registration directly inside their
;; `use-package' form, instead of calling `warmup-add' as a sibling
;; expression.  Reads as a first-class declaration of intent ("warm
;; these features up over idle time"), and stays co-located with
;; the rest of the package's configuration.
;;
;; Equivalent expansions:
;;
;;   (use-package org
;;     :warmup (calendar org-macs ... org-capture))
;;
;;   ;; expands to (roughly):
;;   (progn
;;     (warmup-add '(calendar org-macs ... org-capture))
;;     ...rest of use-package expansion...)
;;
;; Registered via use-package's documented extension protocol --
;; the same one built-in keywords like `:hook' and `:bind' use.

(with-eval-after-load 'use-package-core
  (unless (memq :warmup use-package-keywords)
    ;; Slot `:warmup' right after `:preface' so registration runs
    ;; early in the expansion -- before `:init', `:config', and any
    ;; loader-firing keywords -- and well before `emacs-startup-hook'
    ;; fires.  Mutates `use-package-keywords' in place, the standard
    ;; idiom for use-package extensions.
    (let ((tail (memq :preface use-package-keywords)))
      (setcdr tail (cons :warmup (cdr tail))))

    (defun use-package-normalize/:warmup (_name _keyword args)
      "Validate ARGS as a list of feature symbols for `:warmup'.
use-package collects everything between `:warmup' and the next
keyword into a list, so `:warmup (a b c)' arrives here as
`((a b c))' -- we unwrap one level."
      (let ((features (car args)))
        (unless (and (listp features) (seq-every-p #'symbolp features))
          (error ":warmup wants a list of feature symbols, got %S" features))
        features))

    (defun use-package-handler/:warmup (name _keyword features rest state)
      "Emit `(warmup-add ...)' alongside the rest of the body."
      (use-package-concat
       `((warmup-add ',features))
       (use-package-process-keywords name rest state)))))

(provide 'warmup)
;;; warmup.el ends here
