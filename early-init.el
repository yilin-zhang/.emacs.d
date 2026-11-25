;;; early-init.el --- Pre-init startup optimizations -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Settings that only take effect before package.el initializes and the
;; first frame is created, plus other performance settings.  Most ideas
;; come from doomemacs.
;;
;;; Code:


;; --------------------------------------------------------------
;;           GC: completely disable during startup
;; --------------------------------------------------------------
;; Emacs runs GC once it has allocated more than the larger of
;; `gc-cons-threshold' and `gc-cons-percentage' of the live heap, so
;; raising the threshold is enough.  Something must lower it after startup,
;; or memory use runs away.  `gcmh' (in init-basic.el) does that.
(setq gc-cons-threshold most-positive-fixnum)

;; Load a .el that is newer than its .elc.  This stats both files on every
;; load, so turn it on only after startup.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq load-prefer-newer t))
          100)


;; --------------------------------------------------------------
;;          file-name-handler-alist: empty during startup
;; --------------------------------------------------------------
;; Nearly every file operation matches its path against this alist, which
;; adds up during startup.  Clear it, and restore it afterwards.
(let ((old-value (default-toplevel-value 'file-name-handler-alist)))
  (setq file-name-handler-alist
        ;; An uncompressed `calc-loaddefs.el' means that the built-in
        ;; libraries are not compressed.  Otherwise keep the handler that
        ;; loads .el.gz files.
        (if (locate-file-internal "calc-loaddefs.el" load-path)
            nil
          (list (rassq 'jka-compr-handler old-value))))
  ;; Merge, to keep any handlers that packages added meanwhile.  Depth 101
  ;; runs this after the other hooks.
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist
                    (delete-dups (append file-name-handler-alist old-value))))
            101))


;; --------------------------------------------------------------
;;           Package.el: suppress auto-initialization
;; --------------------------------------------------------------
;; init.el calls `package-initialize' itself.  Setting this there would be
;; too late.
(setq package-enable-at-startup nil)


;; --------------------------------------------------------------
;;        Frame implied resize: the macOS startup trap
;; --------------------------------------------------------------
;; Don't resize the frame when init.el changes the font, which is slow on
;; macOS.
(setq frame-inhibit-implied-resize t)

;; Let the frame size be any number of pixels.
(setq frame-resize-pixelwise t)


;; --------------------------------------------------------------
;;           Startup screen / scratch buffer
;; --------------------------------------------------------------
;; `fundamental-mode' avoids loading the Emacs Lisp mode for the scratch
;; buffer.
(setq inhibit-startup-screen t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; `inhibit-startup-echo-area-message' only works as a literal login name
;; in the init file, so silence the function instead.
(advice-add #'display-startup-echo-area-message :override #'ignore)


;; --------------------------------------------------------------
;;       Disable menu bar / tool bar / scroll bar at the source
;; --------------------------------------------------------------
;; Create the first frame without the bars.  Turning the modes off in
;; init.el would build the bars first and then remove them.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Keep the mode variables in sync with the frame.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Emacs calls this even without a tool bar, and it loads the icons.
(advice-add #'tool-bar-setup :override #'ignore)

;; On macOS, a GUI frame without a menu bar has focus problems, and hiding
;; the bar saves no space.  Keep it there, but not in terminal frames.
(when (eq system-type 'darwin)
  ;; `tty' is a placeholder.  The hook replaces it with 1 on GUI frames and
  ;; 0 on terminal frames.
  (setcdr (assq 'menu-bar-lines default-frame-alist) 'tty)
  (add-hook 'after-make-frame-functions
            (lambda (&optional frame)
              (when (eq (frame-parameter frame 'menu-bar-lines) 'tty)
                (set-frame-parameter frame 'menu-bar-lines
                                     (if (display-graphic-p frame) 1 0))))))


;; --------------------------------------------------------------
;;          Mode line: hide it during startup
;; --------------------------------------------------------------
;; Avoid redrawing it during startup.  `doom-modeline' sets it later.
(setq-default mode-line-format nil)


;; --------------------------------------------------------------
;;              Misc frame / UI defaults
;; --------------------------------------------------------------
(setq ring-bell-function 'ignore)

;; Defaults for `window-divider-mode', enabled in init-basic.el.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)


;; --------------------------------------------------------------
;;        Runtime rendering / interaction optimizations
;; --------------------------------------------------------------

;; Skip the second, case-insensitive pass over `auto-mode-alist'.
(setq auto-mode-case-fold nil)

;; Chinese and English don't need right-to-left text.  BPA, the Bracket
;; Pair Algorithm, resolves bracket direction in mixed-direction text.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Don't draw hollow cursors in other windows.
(setq-default cursor-in-non-selected-windows nil)

;; Scroll faster, at the cost of late highlighting.
(setq fast-but-imprecise-scrolling t)

;; Compacting font caches makes GC slow with large fonts such as CJK and
;; Nerd Fonts.  Not doing so costs a little memory.
(setq inhibit-compacting-font-caches t)

;; Pause fontification while there is pending input.
(setq redisplay-skip-fontification-on-input t)


;; --------------------------------------------------------------
;;                   LSP / subprocess I/O
;; --------------------------------------------------------------
;; Language servers send large messages.  lsp-mode's docs recommend 3MB.
(setq read-process-output-max (* 3 1024 1024))

;;; early-init.el ends here
