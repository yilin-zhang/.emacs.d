;;; ocbud.el --- OpenCode session launcher for Emacs -*- lexical-binding: t; -*-

;; Author: yilinzhang
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience

;;; Commentary:

;; Quickly resume existing OpenCode sessions from ~/.local/share/opencode/opencode.db,
;; or start a new session in a selected directory.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'project)
(require 'term)

(defgroup ocbud nil
  "OpenCode session launcher."
  :group 'tools
  :prefix "ocbud-")

(defcustom ocbud-opencode-db-path
  (expand-file-name "~/.local/share/opencode/opencode.db")
  "Path to OpenCode SQLite database."
  :type 'file
  :group 'ocbud)

(defcustom ocbud-opencode-command "opencode"
  "OpenCode executable name or full path."
  :type 'string
  :group 'ocbud)

(defcustom ocbud-terminal-function #'ocbud-open-in-emacs-terminal
  "Function used to open terminal and run command.
The function is called with two args: DIRECTORY and COMMAND."
  :type 'function
  :group 'ocbud)

(defvar ocbud--completion-directory-map nil
  "Alist from displayed candidate title to session directory.")

(defun ocbud-open-in-emacs-terminal (directory command)
  "Open terminal in Emacs for DIRECTORY and run COMMAND.
Uses `vterm' when available, otherwise falls back to `ansi-term'."
  (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
         (name (file-name-nondirectory (directory-file-name default-directory)))
         (buffer-name (generate-new-buffer-name (format "*ocbud:%s*" name))))
    (if (fboundp 'vterm)
        (let ((buf (vterm buffer-name)))
          (with-current-buffer buf
            (vterm-send-string command)
            (vterm-send-return))
          (pop-to-buffer buf))
      (let* ((shell (or explicit-shell-file-name shell-file-name (getenv "SHELL") "/bin/sh"))
             (buf (ansi-term shell buffer-name)))
        (with-current-buffer buf
          (term-send-raw-string (concat command "\n")))
        (pop-to-buffer buf)))))

(defun ocbud-open-in-iterm (directory command)
  "Open iTerm in DIRECTORY and run COMMAND."
  (unless (eq system-type 'darwin)
    (user-error "ocbud-open-in-iterm is only available on macOS"))
  (let* ((dir (expand-file-name directory))
          (shell (or (getenv "SHELL") "/bin/zsh"))
          (full-command (format "%s -lc %s"
                                (shell-quote-argument shell)
                                (shell-quote-argument
                                 (format "cd %s && %s; exec %s -l"
                                         (shell-quote-argument dir)
                                         command
                                         (shell-quote-argument shell)))))
          (osa (or (executable-find "osascript")
                  (user-error "osascript not found in PATH")))
         (script
          (mapconcat
           #'identity
           (list
            "tell application \"iTerm\""
            (format "set cmd to %s" (prin1-to-string full-command))
            "activate"
            "if (count of windows) = 0 then"
            "set newWindow to (create window with default profile)"
            "tell current session of newWindow"
            "write text cmd"
            "end tell"
            "else"
            "tell current window"
            "create tab with default profile"
            "tell current session"
            "write text cmd"
            "end tell"
            "end tell"
            "end if"
            "end tell")
           "\n")))
    (shell-command
     (format
      "%s -e %s"
      (shell-quote-argument osa)
      (shell-quote-argument script)))))

(defun ocbud--load-sessions ()
  "Return all OpenCode sessions as a list of plists.
Each plist has keys :id, :title and :directory."
  (unless (file-readable-p ocbud-opencode-db-path)
    (user-error "OpenCode database not found: %s" ocbud-opencode-db-path))
  (let ((sqlite3 (executable-find "sqlite3")))
    (unless sqlite3
      (user-error "sqlite3 command not found in PATH"))
    (let* ((sql "SELECT id, title, directory FROM session ORDER BY time_updated DESC;")
           (cmd (format "%s %s %s"
                        (shell-quote-argument sqlite3)
                        (shell-quote-argument ocbud-opencode-db-path)
                        (shell-quote-argument sql)))
           (output (string-trim-right (shell-command-to-string cmd))))
      (if (string-empty-p output)
          nil
        (mapcar
         (lambda (line)
           (let* ((cols (split-string line "|"))
                  (id (car cols))
                  (title (cadr cols))
                  (directory (mapconcat #'identity (cddr cols) "|")))
             (list :id id
                   :title (if (string-empty-p title) "(untitled)" title)
                   :directory (expand-file-name directory))))
         (split-string output "\n" t))))))

(defun ocbud--sql-quote (value)
  "Return SQL single-quoted VALUE with escaped apostrophes."
  (concat "'" (replace-regexp-in-string "'" "''" value t t) "'"))

(defun ocbud--update-session-directory (session-id directory)
  "Update SESSION-ID directory to DIRECTORY in OpenCode database.
Return non-nil when one row was updated."
  (let ((sqlite3 (executable-find "sqlite3")))
    (unless sqlite3
      (user-error "sqlite3 command not found in PATH"))
    (let* ((dir (expand-file-name directory))
           (sql (format "UPDATE session SET directory = %s WHERE id = %s; SELECT changes();"
                        (ocbud--sql-quote dir)
                        (ocbud--sql-quote session-id)))
           (cmd (format "%s %s %s"
                        (shell-quote-argument sqlite3)
                        (shell-quote-argument ocbud-opencode-db-path)
                        (shell-quote-argument sql)))
           (output (string-trim (shell-command-to-string cmd))))
      (string= output "1"))))

(defun ocbud--session-labels (sessions)
  "Build completion labels for SESSIONS.
Labels use title only. Duplicate titles are numbered as (1), (2), (3)."
  (let ((totals (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        labels)
    (dolist (s sessions)
      (let ((title (plist-get s :title)))
        (puthash title (1+ (gethash title totals 0)) totals)))
    (dolist (s sessions)
      (let* ((title (plist-get s :title))
             (total (gethash title totals 0))
             (idx (1+ (gethash title seen 0)))
             (label (if (> total 1)
                        (format "%s (%d)" title idx)
                      title)))
        (puthash title idx seen)
        (push (cons label s) labels)))
    (nreverse labels)))

(defun ocbud--session-annotation (candidate)
  "Return shadow annotation for completion CANDIDATE."
  (let ((dir (alist-get candidate ocbud--completion-directory-map nil nil #'string=)))
    (when dir
      (concat " " (propertize dir 'face 'shadow)))))

(defun ocbud--select-session (sessions prompt)
  "Ask user to select from SESSIONS with PROMPT.
Return the selected session plist."
  (let* ((table (ocbud--session-labels sessions))
         (ocbud--completion-directory-map
          (mapcar (lambda (pair)
                    (cons (car pair) (plist-get (cdr pair) :directory)))
                  table))
         (completion-extra-properties
          '(:annotation-function ocbud--session-annotation))
         (choice (completing-read prompt table nil t)))
    (cdr (assoc choice table))))

(defun ocbud--directory-prefix-p (dir parent)
  "Return non-nil when DIR is within PARENT."
  (string-prefix-p (file-name-as-directory (expand-file-name parent))
                   (file-name-as-directory (expand-file-name dir))))

(defun ocbud--project-scope-directory ()
  "Return project root for current directory, or current directory itself.
If `default-directory' belongs to a project, return that project root;
otherwise return `default-directory'."
  (let* ((proj (project-current nil default-directory))
         (root (when proj (project-root proj))))
    (expand-file-name (or root default-directory))))

(defun ocbud--start-new-session-with-directory-prompt ()
  "Prompt for directory and start a new OpenCode session there."
  (let ((dir (read-directory-name "No session found, choose directory for new OpenCode session: "
                                  default-directory nil t)))
    (funcall ocbud-terminal-function dir ocbud-opencode-command)))

(defun ocbud--resume-session (session)
  "Resume OpenCode SESSION, optionally jumping to its directory first."
  (let* ((session-id (plist-get session :id))
         (directory (plist-get session :directory)))
    (when (y-or-n-p (format "Jump to directory in Emacs first (%s)? " directory))
      (dired directory))
    (funcall ocbud-terminal-function
     directory
     (format "%s -s %s"
             ocbud-opencode-command
             (shell-quote-argument session-id)))))

;;;###autoload
(defun ocbud-update-session-directory ()
  "Update directory of an existing OpenCode session."
  (interactive)
  (let ((sessions (ocbud--load-sessions)))
    (unless sessions
      (user-error "No OpenCode sessions found"))
    (let* ((session (ocbud--select-session sessions "Select OpenCode session to update: "))
           (old-dir (plist-get session :directory))
           (new-dir (read-directory-name (format "New directory (current: %s): " old-dir)
                                         old-dir nil t))
           (session-id (plist-get session :id)))
      (if (ocbud--update-session-directory session-id new-dir)
          (message "Updated session %s directory to %s" session-id (expand-file-name new-dir))
        (user-error "No session updated for id %s" session-id)))))

;;;###autoload
(defun ocbud-open-session ()
  "Choose from all existing OpenCode sessions and resume one.
If there are no sessions, prompt for a directory and start a new one."
  (interactive)
  (let ((sessions (ocbud--load-sessions)))
    (if sessions
        (ocbud--resume-session
         (ocbud--select-session sessions "Select OpenCode session to resume: "))
      (ocbud--start-new-session-with-directory-prompt))))

;;;###autoload
(defun ocbud-open-session-project ()
  "Choose and resume OpenCode session under current project scope.
If current directory belongs to a project, scope is project root and subdirs.
Otherwise scope is current directory and subdirs.
If no matching session exists, prompt for a directory and start a new one."
  (interactive)
  (let* ((sessions (ocbud--load-sessions))
         (scope (ocbud--project-scope-directory))
         (scoped-sessions
          (seq-filter
           (lambda (s)
             (ocbud--directory-prefix-p (plist-get s :directory) scope))
           sessions)))
    (if scoped-sessions
        (ocbud--resume-session
         (ocbud--select-session scoped-sessions
                                (format "Select session in project scope (%s): " scope)))
      (ocbud--start-new-session-with-directory-prompt))))

(define-obsolete-function-alias 'ocbud-open-session-here
  'ocbud-open-session-project "0.1.1")

(provide 'ocbud)

;;; ocbud.el ends here
