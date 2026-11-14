;;; treemacs-fswatch.el --- Feed FSEvents into treemacs through fswatch  -*- lexical-binding: t; -*-

;;; Commentary:

;; On macOS the `file-notify' backend is kqueue.  It reports entries
;; created, deleted or renamed in a watched directory, but not writes to
;; the contents of a file that already exists, so `treemacs-filewatch-mode'
;; never learns that a file became modified unless Emacs itself saved it.
;; It also never looks inside .git, so staging, committing or checking
;; out from a terminal leaves the git faces stale.
;;
;; `treemacs-fswatch-mode' runs a single fswatch process (FSEvents,
;; recursive) over the projects shown in the live treemacs buffers and
;; hands every reported path to treemacs:
;;
;; - a file in an expanded directory becomes a `changed' event for
;;   treemacs' own filewatch handling;
;; - anything under .git, or a file in a collapsed directory, marks the
;;   project for a full refresh, since the faces of the directories above
;;   it may change too;
;; - paths inside git-ignored directories are dropped.
;;
;; Either way the work runs on treemacs' filewatch timer, so a burst of
;; events turns into one refresh after `treemacs-file-event-delay'.

;;; Code:

(require 'seq)
(require 'treemacs)

(defgroup treemacs-fswatch nil
  "Feed fswatch file events into treemacs."
  :group 'treemacs)

(defcustom treemacs-fswatch-executable "fswatch"
  "The fswatch program."
  :type 'string)

(defcustom treemacs-fswatch-latency 0.2
  "Seconds fswatch waits to batch events before reporting them."
  :type 'number)

(defcustom treemacs-fswatch-exclude-regexps
  '("/\\.git/objects/" "/\\.git/logs/" "/node_modules/" "/eln-cache/")
  "Extended regexps of paths fswatch should not report.
Passed to fswatch with -e.  Git-ignored paths are dropped anyway; this
only spares fswatch from reporting large, busy trees at all."
  :type '(repeat regexp))

(defcustom treemacs-fswatch-sync-interval 1
  "Seconds between checks of which directories treemacs shows."
  :type 'number)

(defvar treemacs-fswatch--process nil
  "The running fswatch process, if any.")

(defvar treemacs-fswatch--roots nil
  "Watched project roots, as (TRUENAME . TREEMACS-PATH) pairs.
fswatch reports resolved paths; treemacs keys its nodes by the path the
project was added with.")

(defvar treemacs-fswatch--partial ""
  "Output received after the last newline.")

(defvar treemacs-fswatch--sync-timer nil
  "Repeating timer running `treemacs-fswatch--sync'.")

(defun treemacs-fswatch--wanted-roots ()
  "Return the sorted local project paths shown in live treemacs buffers."
  (let (roots)
    (treemacs-run-in-every-buffer
     (dolist (project (treemacs-workspace->projects (treemacs-current-workspace)))
       (let ((path (treemacs-project->path project)))
         (when (and (stringp path)
                    (not (file-remote-p path))
                    (file-directory-p path))
           (push path roots)))))
    (sort (delete-dups roots) #'string<)))

(defun treemacs-fswatch--stop ()
  "Stop the fswatch process."
  (when (process-live-p treemacs-fswatch--process)
    (delete-process treemacs-fswatch--process))
  (setq treemacs-fswatch--process nil
        treemacs-fswatch--roots nil
        treemacs-fswatch--partial ""))

(defun treemacs-fswatch--start (roots)
  "Start one fswatch process watching ROOTS."
  (setq treemacs-fswatch--roots
        (mapcar (lambda (root)
                  (cons (file-name-as-directory (file-truename root))
                        (file-name-as-directory root)))
                roots))
  (setq treemacs-fswatch--process
        (make-process
         :name "treemacs-fswatch"
         :command `(,treemacs-fswatch-executable
                    "--recursive"
                    "--latency" ,(number-to-string treemacs-fswatch-latency)
                    ,@(mapcan (lambda (re) (list "--exclude" re))
                              treemacs-fswatch-exclude-regexps)
                    ,@(mapcar #'car treemacs-fswatch--roots))
         :connection-type 'pipe
         :noquery t
         :filter #'treemacs-fswatch--filter
         :sentinel #'ignore)))

(defun treemacs-fswatch--sync ()
  "Point fswatch at the directories treemacs currently shows.
A process that died is only restarted once that set changes, so a
broken fswatch is not respawned every interval."
  (let ((roots (treemacs-fswatch--wanted-roots)))
    (unless (equal roots (mapcar (lambda (r) (directory-file-name (cdr r)))
                                 treemacs-fswatch--roots))
      (treemacs-fswatch--stop)
      (when roots
        (treemacs-fswatch--start roots)))))

(defun treemacs-fswatch--git-ignored-p (path root)
  "Return non-nil if PATH, or a directory above it below ROOT, is git-ignored.
Read from treemacs' git cache, so only what treemacs has already seen
counts."
  (let ((found nil))
    (while (and (not found)
                (string-prefix-p root path)
                (not (equal (file-name-as-directory path) root)))
      (let ((cache (ht-get treemacs--git-cache (treemacs--parent-dir path))))
        (if (and cache (eq 'treemacs-git-ignored-face (ht-get cache path)))
            (setq found t)
          (setq path (treemacs--parent-dir path)))))
    found))

(defun treemacs-fswatch--mark-project (root)
  "Mark project ROOT for a full refresh in every treemacs buffer."
  (let ((key (directory-file-name root)))
    (treemacs-run-in-every-buffer
     (when-let* ((node (treemacs-find-in-dom key)))
       (unless (assoc key (treemacs-dom-node->refresh-flag node))
         (push (cons key 'force-refresh)
               (treemacs-dom-node->refresh-flag node)))
       (treemacs--start-filewatch-timer)))))

(defun treemacs-fswatch--handle (truename)
  "Hand the change fswatch reported at TRUENAME to treemacs."
  (when-let* ((root (seq-find (lambda (r) (string-prefix-p (car r) truename))
                              treemacs-fswatch--roots))
              (path (directory-file-name
                     (concat (cdr root) (substring truename (length (car root))))))
              ((not (equal (file-name-as-directory path) (cdr root)))))
    (cond
     ((string-match-p "/\\.git\\(?:/\\|\\'\\)" path)
      (treemacs-fswatch--mark-project (cdr root)))
     ((treemacs-fswatch--git-ignored-p path (cdr root)))
     ;; Creations and deletions already reach treemacs through kqueue,
     ;; and a directory's own entry carries no git state.
     ((not (file-regular-p path)))
     ((ht-get treemacs--filewatch-index (treemacs--parent-dir path))
      (treemacs--filewatch-callback (list nil 'changed path)))
     (t
      (treemacs-fswatch--mark-project (cdr root))))))

(defun treemacs-fswatch--filter (_process output)
  "Split fswatch OUTPUT into paths and handle each."
  (let ((lines (split-string (concat treemacs-fswatch--partial output) "\n")))
    (setq treemacs-fswatch--partial (car (last lines)))
    (dolist (line (butlast lines))
      (unless (string-empty-p line)
        (treemacs-fswatch--handle line)))))

(defun treemacs-fswatch--refind-button (fn btn path buffer git-future)
  "Call FN with PATH's current button in place of a stale BTN.
Advises `treemacs--apply-annotations-deferred', whose timer keeps BTN
as a buffer position.  A refresh before it fires leaves other text there."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless (and (<= (point-min) btn (point-max))
                   (equal (treemacs-button-get btn :path) path))
        (setq btn (treemacs-find-visible-node path))
        ;; No longer shown: let FN update the git cache, skip the buffer.
        (unless btn (setq buffer nil)))))
  (funcall fn btn path buffer git-future))

;;;###autoload
(define-minor-mode treemacs-fswatch-mode
  "Refresh treemacs from fswatch (FSEvents) file events.
Watches the projects shown in live treemacs buffers with one fswatch
process, following them as they change."
  :global t
  :group 'treemacs-fswatch
  (when (timerp treemacs-fswatch--sync-timer)
    (cancel-timer treemacs-fswatch--sync-timer))
  (setq treemacs-fswatch--sync-timer nil)
  (treemacs-fswatch--stop)
  (advice-remove 'treemacs--apply-annotations-deferred
                 #'treemacs-fswatch--refind-button)
  (when treemacs-fswatch-mode
    (if (not (executable-find treemacs-fswatch-executable))
        (progn
          (setq treemacs-fswatch-mode nil)
          (message "treemacs-fswatch: %s not found" treemacs-fswatch-executable))
      (advice-add 'treemacs--apply-annotations-deferred :around
                  #'treemacs-fswatch--refind-button)
      (setq treemacs-fswatch--sync-timer
            (run-with-timer 0 treemacs-fswatch-sync-interval
                            #'treemacs-fswatch--sync)))))

(provide 'treemacs-fswatch)

;;; treemacs-fswatch.el ends here
