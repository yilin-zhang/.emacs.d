;;; init-basic.el --- Basic configurations. -*- lexical-binding: t -*-

;;; Commentary:
;; Configure core performance, appearance, and user-interface behavior.

;;; Code:

;; --------------------------------------------------------------
;;                         Performance
;; --------------------------------------------------------------
;; settings from Doom Emacs
(use-package gcmh
  :hook emacs-startup
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 64 1024 1024))) ; 64MB

;; Load heavy packages while Emacs is idle, so that their first use doesn't
;; pause.  Packages opt in with the `:warmup' keyword, and loading this
;; package sets everything up.
(use-package warmup
  :load-path yilin/site-lisp-directory
  :demand t)

;; --------------------------------------------------------------
;;                        Basic Features
;; --------------------------------------------------------------
(use-package emacs
  :ensure nil
  :preface
  (defun yilin/display-startup-time ()
    (message "Emacs loaded in %s with %d garbage collections."
             (format "%.2f seconds"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
             gcs-done))
  :init
  ;; Must be set before `display-time-mode' is enabled.
  (setopt display-time-string-forms
          '((propertize (concat 24-hours ":" minutes " ")
                        'face 'font-lock-constant-face)))
  ;; Confirmation
  (setopt use-short-answers t)
  (setopt confirm-kill-emacs 'yes-or-no-p)
  ;; Editing
  (setopt major-mode 'text-mode
          fill-column 80
          truncate-lines t)
  ;; TAB indents first, then completes.
  (setopt tab-always-indent 'complete)
  ;; Cursor and scrolling
  (setopt mouse-wheel-scroll-amount '(1 ((shift) . 1)) ; one line at a time
          mouse-wheel-progressive-speed nil            ; don't accelerate scrolling
          scroll-conservatively 100000                  ; keyboard scroll one line at a time
          scroll-preserve-screen-position 'always)      ; lock cursor position when scrolling
  ;; File saving
  (setopt make-backup-files nil  ; disable backup file
          auto-save-default nil) ; disable auto-save
  ;; Resolve symlinks when visiting files.
  (setopt find-file-visit-truename t)
  ;; Warning handling
  (setopt warning-suppress-types '((emacs)))
  ;; Borrowed from emacsredux.
  (setopt set-mark-command-repeat-pop t  ; keep popping the mark ring with C-SPC
          help-window-select t           ; focus help/helpful windows when they open
          ffap-machine-p-known 'reject   ; never ping hosts in find-file-at-point
          reb-re-syntax 'string)         ; readable string syntax in `re-builder'
  :hook
  (after-init . global-so-long-mode)
  (after-init . delete-selection-mode)
  (after-init . global-hl-line-mode) ; highlight the current line
  (after-init . pixel-scroll-precision-mode)
  (window-setup . window-divider-mode)
  (emacs-startup . yilin/display-startup-time)
  )

;; --------------------------------------------------------------
;;                              Icons
;; --------------------------------------------------------------
(use-package nerd-icons)

;; --------------------------------------------------------------
;;                        Theme and Modeline
;; --------------------------------------------------------------
(use-package doom-themes
  :demand t
  :config
  ;; doom-gruvbox with the colors of the JetBrains Gruvbox plugin.  Set
  ;; `gruvbox-jb-variant' for the hard or soft background.
  (add-to-list 'custom-theme-load-path yilin/site-lisp-directory)
  (load-theme 'gruvbox-jb t)
  ;; The visual bell selects the wrong buffer when used with dired-async.
  ;; (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :hook after-init
  :custom
  (doom-modeline-support-imenu t)
  (doom-modeline-modal-icon nil))

;; --------------------------------------------------------------
;;                            Paths
;; --------------------------------------------------------------
(use-package exec-path-from-shell
  ;; In a terminal Emacs already inherits the shell's environment.
  :if (memq window-system '(mac ns x))
  :custom
  ;; Leave out `-i', which would load the slow ~/.zshrc.  Environment
  ;; variables belong in ~/.zprofile.
  (exec-path-from-shell-arguments '("-l"))
  :hook (after-init . exec-path-from-shell-initialize))

;; mise manages the Ruby tools, but only an interactive shell puts them on
;; PATH.  Its shims pick the tool version from `default-directory', so each
;; project gets its own Ruby.  Depth 90 runs this after
;; `exec-path-from-shell-initialize', which replaces `exec-path'.
(defun yilin/add-mise-shims-to-path ()
  "Add mise's shim directory to `exec-path' and PATH."
  (let ((dir (expand-file-name "~/.local/share/mise/shims")))
    (when (file-directory-p dir)
      (add-to-list 'exec-path dir)
      (setenv "PATH" (concat dir path-separator (getenv "PATH"))))))

(add-hook 'after-init-hook #'yilin/add-mise-shims-to-path 90)

;; --------------------------------------------------------------
;;                            Server
;; --------------------------------------------------------------
(use-package server
  :ensure nil
  :hook (after-init . (lambda ()
                        ;; Another Emacs may already own the server.
                        (require 'server)
                        (unless (server-running-p)
                          (server-start)))))

;; Emacs can't move iCloud Drive files to the trash on macOS ("Attribute not
;; found"), and neither can trash(1), but Finder can.  The advice is on
;; `move-file-to-trash' because natively compiled code bypasses advice on
;; the `system-move-file-to-trash' primitive.
(when (eq system-type 'darwin)
  (defun yilin/trash-with-macos-fallback (orig file)
    "Call ORIG on FILE; on `file-error', retry by asking Finder."
    (condition-case err
        (funcall orig file)
      (file-error
       (unless (and (not (file-remote-p file))
                    (zerop (call-process
                            "osascript" nil nil nil
                            "-e" "on run argv"
                            "-e" "tell application \"Finder\" to delete ((POSIX file (item 1 of argv)) as alias)"
                            "-e" "end run"
                            (expand-file-name file))))
         (signal (car err) (cdr err))))))
  (advice-add 'move-file-to-trash :around
              #'yilin/trash-with-macos-fallback))

;; --------------------------------------------------------------
;;                            Window
;; --------------------------------------------------------------
(use-package emacs
  :preface
  ;; Full Screen with Mode Line Time Display
  (defun yilin/toggle-frame-fullscreen ()
    "toggle-frame-fullscreen plus display-time-mode."
    (interactive)
    (if (equal 'fullboth (frame-parameter nil 'fullscreen))
        (progn
          (display-time-mode -1)
          (display-battery-mode -1))
      (progn
        (display-time-mode 1)
        (display-battery-mode 1)))
    (toggle-frame-fullscreen))

  :bind
  ;; Focus on the new window after splitting
  ([remap split-window-right] . (lambda () (interactive) (split-window-right) (other-window 1)))
  ([remap split-window-below] . (lambda () (interactive) (split-window-below) (other-window 1)))
  ("<f12>" . yilin/toggle-frame-fullscreen)
  ("C-x |" . window-layout-rotate-clockwise)
  )

(use-package popwin
  :hook after-init)

;; --------------------------------------------------------------
;;                          Auto Revert
;; --------------------------------------------------------------
(use-package lazy-revert
  :load-path yilin/site-lisp-directory
  :preface
  (defun yilin/revert-all-buffers ()
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (let ((auto-revert-mode t))
          (auto-revert-handler)))))
  :hook after-init
  :custom
  ;; Files shown in windows are watched, so don't poll them as well.
  (auto-revert-avoid-polling t)
  ;; Replacing a file by rename drops its watch, and the buffer is polled
  ;; until the watch is back.  Few buffers ever are, so poll often.
  (auto-revert-interval 1)
  (auto-revert-stop-on-user-input nil)
  ;; Only prompts for confirmation when buffer is unsaved.
  (revert-without-query (list ".")))

;; --------------------------------------------------------------
;;                              Files
;; --------------------------------------------------------------
(use-package recentf
  :ensure nil
  :hook after-init
  :custom
  (recentf-max-saved-items 50))

;; Remember and restore point's position in each visited file.
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode)
  :config
  ;; Recenter the window after save-place jumps to the restored position.
  (advice-add 'save-place-find-file-hook :after
              (lambda (&rest _)
                (when buffer-file-name (ignore-errors (recenter))))))

;; --------------------------------------------------------------
;;                             Buffer
;; --------------------------------------------------------------
;; use ibuffer instead of CRM buffer
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

(use-package nerd-icons-ibuffer
  :after ibuffer
  :hook ibuffer-mode)

;; --------------------------------------------------------------
;;                              Fonts
;; --------------------------------------------------------------

(use-package emacs
  :ensure nil
  :preface
  (defvar yilin/default-font-size 15 "The default font size")
  (defvar yilin/fixed-pitch-font "Sarasa Term SC" "The fixed pitch font")
  (defvar yilin/variable-pitch-font "Noto Serif" "The variable pitch font")

  (defun yilin/set-fonts (&optional font-size)
    "Set fonts with the given FONT-SIZE."
    (interactive (list (read-number "Enter size: " yilin/default-font-size)))
    (let ((actual-font-size (if (and font-size (< 0 font-size))
                                font-size
                              yilin/default-font-size)))
      (set-face-attribute 'default nil :font (font-spec :family yilin/fixed-pitch-font :size actual-font-size))
      (set-face-attribute 'fixed-pitch nil :font (font-spec :family yilin/fixed-pitch-font :size actual-font-size))
      (if (find-font (font-spec :family yilin/variable-pitch-font))
          (set-face-attribute 'variable-pitch nil :font (font-spec :family yilin/variable-pitch-font :size (+ 2 actual-font-size)))
        (message "Font %s not found, keeping default variable-pitch" yilin/variable-pitch-font))
      ;; Fallback for icon glyphs.
      (when (find-font (font-spec :family "Symbols Nerd Font Mono"))
        (set-fontset-font t 'symbol "Symbols Nerd Font Mono" nil 'append)
        (set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'append))))

  (defun yilin/set-variable-pitch ()
    "Set current buffer's font to variable-pitch"
    (buffer-face-set 'variable-pitch))

  :init
  (when (display-graphic-p)
    (yilin/set-fonts)))


;; --------------------------------------------------------------
;;                            Editing
;; --------------------------------------------------------------

(defconst yilin/han-regexp
  (rx (in (#x3400 . #x4DBF)     ; CJK Unified Ideographs Extension A
          (#x4E00 . #x9FFF)     ; CJK Unified Ideographs
          (#xF900 . #xFAFF)     ; CJK Compatibility Ideographs
          (#x20000 . #x2A6DF))) ; CJK Unified Ideographs Extension B
  "Regexp matching a single Han character.
Unlike `\\cc', it doesn't match full-width punctuation.")

(defun yilin/space-han-latin (&optional beg end)
  "Separate Han characters from adjacent Latin letters and digits.
Act on the region when one is active, on the whole buffer otherwise.
BEG and END bound the text to walk when called from Lisp."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (let* ((beg (or beg (point-min)))
         ;; A marker, because every insertion shifts the end.
         (end (copy-marker (or end (point-max))))
         (latin "[A-Za-z0-9]")
         (added 0))
    (save-excursion
      (dolist (pattern (list (concat "\\(" yilin/han-regexp "\\)\\(" latin "\\)")
                             (concat "\\(" latin "\\)\\(" yilin/han-regexp "\\)")))
        (goto-char beg)
        (while (re-search-forward pattern end t)
          (replace-match "\\1 \\2")
          (setq added (1+ added)))))
    (set-marker end nil)
    (message "Inserted %d space%s" added (if (= added 1) "" "s"))))

;; --------------------------------------------------------------
;;                            Terminal
;; --------------------------------------------------------------

;; Emacs asks the terminal to report Ctrl keys as escape sequences
;; (modifyOtherKeys), but doesn't decode those for C-[ and C-], so C-[ would
;; insert "[91;5u".  Decode both the CSI-u form that Ghostty sends and the
;; xterm form.
;;
;; C-[ becomes `escape', because a plain ESC would bypass the filter that
;; `meow-esc-mode' installs in `input-decode-map'.  C-[ still works as the
;; Meta prefix, through `function-key-map'.
(defconst yilin/xterm-csi-u-control-keys
  '((91 . escape)       ; C-[  ->  <escape>
    (93 . ?\C-\]))       ; C-]  ->  GS
  "Alist of codepoints of Ctrl keys that Emacs doesn't decode, and their keys.")

(defun yilin/xterm-decode-csi-u-control-keys ()
  "Decode the keys in `yilin/xterm-csi-u-control-keys'."
  (pcase-dolist (`(,code . ,key) yilin/xterm-csi-u-control-keys)
    ;; 5 is the modifier encoding for Ctrl (1 + 4).
    (define-key input-decode-map (format "\e[%d;5u" code) (vector key))
    (define-key input-decode-map (format "\e[27;5;%d~" code) (vector key))))

(add-hook 'terminal-init-xterm-hook #'yilin/xterm-decode-csi-u-control-keys)

(provide 'init-basic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
