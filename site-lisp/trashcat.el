;;; trashcat.el --- Clean up macOS applications and their residual files -*- lexical-binding: t -*-

;; Copyright (C) 2024 trashcat authors

;; Author: Converted from macOS App Cleaner
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools
;; URL: TODO

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Trashcat is an Emacs package for thoroughly removing macOS applications
;; and their residual files.  It provides a user-friendly interface to
;; find and remove application bundles along with associated files from
;; common locations such as Library/Application Support, Library/Caches,
;; Library/Preferences, etc.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;; Define file record structure
(cl-defstruct (trashcat-file
               (:constructor trashcat-file-create)
               (:copier nil))
  id          ; Unique ID for the file
  type        ; Type of file (e.g., "App Bundle", "Application Support")
  path        ; Full path to the file
  size        ; Size in bytes
  selected)   ; Boolean indicating if selected for removal

(defgroup trashcat nil
  "Settings for the Trashcat application cleaner."
  :group 'tools
  :prefix "trashcat-")

(defcustom trashcat-residual-locations
  '(("Application Support" . "Library/Application Support")
    ("Caches" . "Library/Caches")
    ("Preferences" . "Library/Preferences")
    ("Logs" . "Library/Logs")
    ("Saved Application State" . "Library/Saved Application State")
    ("Containers" . "Library/Containers")
    ("Group Containers" . "Library/Group Containers"))
  "Common locations for app residual files."
  :type '(alist :key-type string :value-type string)
  :group 'trashcat)

(defcustom trashcat-app-locations
  '("/Applications"
    "~/Applications")
  "Common locations for macOS applications."
  :type '(repeat directory)
  :group 'trashcat)

;; Internal variables
(defvar trashcat--app-name nil
  "Name of the application being processed.")

(defvar trashcat--bundle-id nil
  "Bundle identifier of the application.")

(defvar trashcat--files nil
  "List of files found for the current application.")

(defvar trashcat--buffer-name "*Trashcat*"
  "Name of the Trashcat buffer.")

(defvar trashcat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'trashcat-toggle-at-point)
    (define-key map (kbd "SPC") 'trashcat-toggle-at-point)
    (define-key map (kbd "a") 'trashcat-select-all)
    (define-key map (kbd "n") 'trashcat-select-none)
    (define-key map (kbd "r") 'trashcat-remove-selected)
    (define-key map (kbd "g") 'trashcat-refresh)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "?") 'trashcat-help)
    map)
  "Keymap for `trashcat-mode'.")

(define-derived-mode trashcat-mode special-mode "Trashcat"
  "Major mode for the Trashcat app cleaner.
\\{trashcat-mode-map}"
  :group 'trashcat
  (buffer-disable-undo)
  (setq truncate-lines t
        buffer-read-only t))

(defface trashcat-app-name-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for application name.")

(defface trashcat-path-face
  '((t :inherit font-lock-string-face))
  "Face for file paths.")

(defface trashcat-type-face
  '((t :inherit font-lock-type-face))
  "Face for file types.")

(defface trashcat-size-face
  '((t :inherit font-lock-constant-face))
  "Face for file sizes.")

(defface trashcat-selected-face
  '((t :inherit success))
  "Face for selected marker.")

(defface trashcat-unselected-face
  '((t :inherit error))
  "Face for unselected marker.")

(defface trashcat-header-face
  '((t :inherit font-lock-keyword-face :height 1.2 :weight bold))
  "Face for headers.")

(defface trashcat-total-face
  '((t :inherit font-lock-preprocessor-face :weight bold))
  "Face for total size information.")

(defface trashcat-button-face
  '((t :inherit font-lock-builtin-face :weight bold :box t))
  "Face for buttons.")

;;; Utility functions

(defun trashcat--generate-id ()
  "Generate a unique ID for file records."
  (format "%s" (random)))

(defun trashcat--get-bundle-identifier (app-path)
  "Extract the bundle identifier from the app's Info.plist at APP-PATH."
  (let ((info-plist (expand-file-name "Contents/Info.plist" app-path)))
    (when (file-exists-p info-plist)
      (with-temp-buffer
        (call-process "plutil" nil t nil "-convert" "xml1" "-o" "-" info-plist)
        (goto-char (point-min))
        (when (re-search-forward "<key>CFBundleIdentifier</key>\\s-*<string>\\([^<]+\\)</string>" nil t)
          (setq trashcat--bundle-id (match-string 1)))))))

(defun trashcat--format-size (size-bytes)
  "Format SIZE-BYTES to human-readable size."
  (if (= size-bytes 0)
      "0B"
    (let ((size-names '("B" "KB" "MB" "GB" "TB"))
          (size size-bytes)
          (i 0))
      (while (and (>= size 1024) (< i (1- (length size-names))))
        (setq size (/ size 1024.0)
              i (1+ i)))
      (format "%.2f %s" size (nth i size-names)))))

(defun trashcat--get-file-size (path)
  "Get the size of a file or directory at PATH in bytes."
  (unless (file-exists-p path)
    (error "File does not exist: %s" path))

  (if (file-regular-p path)
      (file-attribute-size (file-attributes path))
    ;; For directories, use du command
    (with-temp-buffer
      (if (= 0 (call-process "du" nil t nil "-sk" path))
          (progn
            (goto-char (point-min))
            (when (re-search-forward "^\\([0-9]+\\)" nil t)
              (* 1024 (string-to-number (match-string 1)))))
        ;; Fallback if du fails
        (let ((default-directory path)
              (total-size 0))
          (dolist (file (directory-files-recursively path ".*" t))
            (when (file-regular-p file)
              (setq total-size (+ total-size (file-attribute-size (file-attributes file))))))
          total-size)))))

;;; Core functionality

(defun trashcat-find-app-bundle (app-name)
  "Find the application bundle path for APP-NAME."
  (let (app-path)
    ;; Try exact match first
    (catch 'found
      (dolist (location trashcat-app-locations)
        (let ((expanded-location (expand-file-name location)))
          (when (file-directory-p expanded-location)
            (let ((potential-path (expand-file-name (format "%s.app" app-name) expanded-location)))
              (when (file-directory-p potential-path)
                (trashcat--get-bundle-identifier potential-path)
                (setq app-path potential-path)
                (throw 'found t))))))

      ;; If not found, try case-insensitive search
      (dolist (location trashcat-app-locations)
        (let ((expanded-location (expand-file-name location)))
          (when (file-directory-p expanded-location)
            (dolist (app (directory-files expanded-location))
              (when (and (string-match-p "\\.app$" app)
                         (string-match-p (regexp-quote (downcase app-name)) (downcase app)))
                (let ((potential-path (expand-file-name app expanded-location)))
                  (trashcat--get-bundle-identifier potential-path)
                  (setq app-path potential-path)
                  (throw 'found t))))))))
    app-path))

(defun trashcat-find-all-related-files (app-name)
  "Find all files related to APP-NAME, including app bundle and residual files."
  (let ((related-files nil)
        (app-bundle (trashcat-find-app-bundle app-name))
        (app-name-no-spaces (replace-regexp-in-string " " "" app-name)))

    ;; Add app bundle if found
    (when app-bundle
      (push (trashcat-file-create
             :id (trashcat--generate-id)
             :type "App Bundle"
             :path app-bundle
             :size (trashcat--get-file-size app-bundle)
             :selected t)
            related-files))

    ;; Find residual files in common locations
    (dolist (location trashcat-residual-locations)
      (let* ((location-name (car location))
             (location-path (expand-file-name (cdr location) "~"))
             possible-matches)

        (when (file-directory-p location-path)
          (dolist (item (directory-files location-path))
            (let ((item-path (expand-file-name item location-path)))
              ;; Match by name patterns
              (when (or (string-match-p (regexp-quote (downcase app-name)) (downcase item))
                        (string-match-p (regexp-quote (downcase app-name-no-spaces)) (downcase item))
                        (and trashcat--bundle-id
                             (string-match-p (regexp-quote (downcase trashcat--bundle-id)) (downcase item))))
                (push item-path possible-matches))))

          ;; Add all matches as separate items
          (dolist (match possible-matches)
            (push (trashcat-file-create
                   :id (trashcat--generate-id)
                   :type location-name
                   :path match
                   :size (trashcat--get-file-size match)
                   :selected t)
                  related-files)))))

    (nreverse related-files)))

(defun trashcat-move-to-trash (path)
  "Move PATH to the trash using macOS's `trash` command or Emacs' delete-by-moving-to-trash."
  (cond
   ;; Try using macOS built-in trash command if available
   ((executable-find "trash")
    (= 0 (call-process "trash" nil nil nil path)))

   ;; Try using AppleScript (always available on macOS)
   (t
    (= 0 (call-process "osascript" nil nil nil "-e"
                       (format "tell application \"Finder\" to delete POSIX file \"%s\"" path))))))

(defun trashcat-remove-selected-files (selected-files)
  "Move all files in SELECTED-FILES list to the trash."
  (let ((success t)
        (removed-count 0))
    (dolist (file selected-files)
      (let ((path (trashcat-file-path file)))
        (when (file-exists-p path)
          (condition-case err
              (progn
                (if (trashcat-move-to-trash path)
                    (cl-incf removed-count)
                  (message "Failed to move %s to trash" path)
                  (setq success nil))
                )
            (error
             (message "Error moving %s to trash: %s" path (error-message-string err))
             (setq success nil))))))
    (cons success removed-count)))

;;; UI functions

(defun trashcat-render-buffer ()
  "Render the Trashcat buffer with files list."
  (let ((inhibit-read-only t)
        (point-pos (point))
        (window-start (window-start))
        (selected-files (seq-filter #'trashcat-file-selected trashcat--files))
        (total-size (seq-reduce (lambda (acc f) (+ acc (trashcat-file-size f))) trashcat--files 0))
        (selected-size (seq-reduce (lambda (acc f)
                                     (+ acc (if (trashcat-file-selected f)
                                                (trashcat-file-size f) 0)))
                                   trashcat--files 0)))
    (erase-buffer)

    ;; Header
    (insert (propertize (format "Trashcat: Clean up %s\n\n" trashcat--app-name)
                        'face 'trashcat-header-face))

    ;; Instructions
    (insert "Commands: ")
    (insert (propertize "SPC/RET" 'face 'trashcat-button-face) " Toggle selection, ")
    (insert (propertize "a" 'face 'trashcat-button-face) " Select all, ")
    (insert (propertize "n" 'face 'trashcat-button-face) " Select none, ")
    (insert (propertize "r" 'face 'trashcat-button-face) " Move selected to trash, ")
    (insert (propertize "g" 'face 'trashcat-button-face) " Refresh, ")
    (insert (propertize "q" 'face 'trashcat-button-face) " Quit, ")
    (insert (propertize "?" 'face 'trashcat-button-face) " Help\n\n")

    ;; File list
    (insert (propertize "Files related to " 'face 'font-lock-comment-face))
    (insert (propertize trashcat--app-name 'face 'trashcat-app-name-face))
    (insert (propertize ":\n\n" 'face 'font-lock-comment-face))

    (insert (propertize (format "%-3s %-8s %-20s %-10s %s\n"
                                "#" "Selected" "Type" "Size" "Path")
                        'face 'font-lock-comment-face))
    (insert (make-string (window-width) ?-) "\n")

    (let ((i 1))
      (dolist (file trashcat--files)
        (let* ((selected-mark (if (trashcat-file-selected file) "✓" "✗"))
               (file-size (trashcat--format-size (trashcat-file-size file)))
               (line-start (point))
               (type-column (format "%-20s " (trashcat-file-type file)))
               (size-column (format "%-10s " file-size))
               (path-column (trashcat-file-path file)))

          ;; Insert line number
          (insert (format "%-3d " i))

          ;; Insert selection mark with appropriate face
          (let ((mark-start (point)))
            (insert (format "%-8s " selected-mark))
            (put-text-property
             mark-start (+ mark-start 1)
             'face (if (trashcat-file-selected file)
                       'trashcat-selected-face
                     'trashcat-unselected-face)))

          ;; Insert type, size, and path
          (insert type-column)
          (insert size-column)
          (insert path-column)
          (insert "\n")

          ;; Add text properties to the entire line
          (add-text-properties
           line-start (point)
           (list 'trashcat-file file
                 'trashcat-index i))

          ;; Add face properties to specific parts
          ;; Type column
          (let ((type-start (- (point) (length path-column) (length size-column) (length type-column) 1)))
            (put-text-property
             type-start (+ type-start (length type-column))
             'face 'trashcat-type-face))

          ;; Size column
          (let ((size-start (- (point) (length path-column) (length size-column) 1)))
            (put-text-property
             size-start (+ size-start (length size-column))
             'face 'trashcat-size-face))

          ;; Path column
          (put-text-property
           (- (point) (length path-column) 1) (point)
           'face 'trashcat-path-face)

          (setq i (1+ i)))))

    ;; Summary footer
    (insert (make-string (window-width) ?-) "\n")
    (insert (propertize (format "Total size: %s\n"
                                (trashcat--format-size total-size))
                        'face 'trashcat-total-face))
    (insert (propertize (format "Selected size: %s (%d files)\n"
                                (trashcat--format-size selected-size)
                                (length selected-files))
                        'face 'trashcat-total-face))

    ;; Restore point and window start
    (set-window-start (selected-window) window-start)
    (goto-char point-pos)))

(defun trashcat-toggle-at-point ()
  "Toggle selection of the file at point."
  (interactive)
  (let ((file (get-text-property (point) 'trashcat-file)))
    (when file
      ;; Toggle the selected status
      (setf (trashcat-file-selected file)
            (not (trashcat-file-selected file)))
      ;; Force buffer redisplay
      (trashcat-render-buffer)
      ;; Report the change
      (message "%s %s"
               (if (trashcat-file-selected file) "Selected" "Deselected")
               (trashcat-file-path file)))))

(defun trashcat-select-all ()
  "Select all files in the list."
  (interactive)
  (dolist (file trashcat--files)
    (setf (trashcat-file-selected file) t))
  (trashcat-render-buffer)
  (message "All files selected"))

(defun trashcat-select-none ()
  "Deselect all files in the list."
  (interactive)
  (dolist (file trashcat--files)
    (setf (trashcat-file-selected file) nil))
  (trashcat-render-buffer)
  (message "All files deselected"))

(defun trashcat-refresh ()
  "Refresh the list of files."
  (interactive)
  (setq trashcat--files (trashcat-find-all-related-files trashcat--app-name))
  (trashcat-render-buffer)
  (message "File list refreshed"))

(defun trashcat-remove-selected ()
  "Move all selected files to trash."
  (interactive)
  (let* ((selected-files (seq-filter #'trashcat-file-selected trashcat--files))
         (selected-size (seq-reduce (lambda (acc f) (+ acc (trashcat-file-size f)))
                                    selected-files 0))
         (file-count (length selected-files)))

    (if (null selected-files)
        (message "No files selected for trash")
      (when (yes-or-no-p
             (format "Move %d selected files (%s) to trash? "
                     file-count (trashcat--format-size selected-size)))
        (let ((result (trashcat-remove-selected-files selected-files)))
          (if (car result)
              (progn
                (message "Successfully moved %d files to trash" (cdr result))
                (trashcat-refresh))
            (message "Some files could not be moved to trash")))))))

(defun trashcat-help ()
  "Show help for Trashcat mode."
  (interactive)
  (with-help-window "*Trashcat Help*"
    (princ "Trashcat: Clean up macOS applications and their residual files\n\n")
    (princ "Key bindings:\n")
    (princ "  SPC, RET  Toggle selection of file at point\n")
    (princ "  a         Select all files\n")
    (princ "  n         Deselect all files\n")
    (princ "  r         Move selected files to trash\n")
    (princ "  g         Refresh file list\n")
    (princ "  q         Quit Trashcat\n")
    (princ "  ?         Show this help\n\n")
    (princ "Trashcat scans for and helps you move to trash:\n")
    (princ "- Application bundles (.app)\n")
    (princ "- Residual files from common locations:\n")
    (dolist (loc trashcat-residual-locations)
      (princ (format "  - %s\n" (car loc))))))

(defun trashcat-get-app-list ()
  "Get list of installed applications from common locations."
  (let ((app-list nil))
    (dolist (location trashcat-app-locations)
      (let ((expanded-location (expand-file-name location)))
        (when (file-directory-p expanded-location)
          (dolist (app (directory-files expanded-location))
            (when (and (string-match-p "\\.app$" app)
                       (file-directory-p (expand-file-name app expanded-location)))
              (push (substring app 0 -4) app-list))))))
    (sort app-list #'string<)))

;;;###autoload
(defun trashcat (app-name)
  "Start Trashcat to clean up APP-NAME and its residual files.
When called interactively with no prefix argument, prompt for an application
from a list. With prefix argument, prompt for direct input of application name."
  (interactive
   (list
    (if current-prefix-arg
        (read-string "Application name: ")
      (let ((apps (trashcat-get-app-list)))
        (if apps
            (completing-read "Select application: " apps nil t)
          (read-string "Application name: "))))))

  (setq trashcat--app-name app-name
        trashcat--bundle-id nil)

  (let ((app-bundle (trashcat-find-app-bundle app-name)))
    (unless app-bundle
      (user-error "Could not find application '%s'" app-name))

    (message "Found app bundle: %s" app-bundle)
    (setq trashcat--files (trashcat-find-all-related-files app-name))

    (if (null trashcat--files)
        (user-error "No files found for '%s'" app-name)

      ;; Switch to or create Trashcat buffer
      (let ((buffer (get-buffer-create trashcat--buffer-name)))
        (switch-to-buffer buffer)
        (trashcat-mode)
        (trashcat-render-buffer)))))

(provide 'trashcat)
;;; trashcat.el ends here
