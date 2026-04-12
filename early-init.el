;;; early-init.el --- Pre-init startup optimizations -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; This file is loaded by Emacs *before* init.el. Specifically, it runs:
;;   1. Before package.el's auto-initialization
;;   2. Before the initial frame (menu bar / tool bar / scroll bar) is created
;;   3. Before the GUI framework finishes initializing
;;
;; Many startup optimizations can ONLY be done here. By the time init.el
;; runs, package.el is up, the initial frame is drawn, and the chance to
;; "just not do the work" is gone -- all we can do is undo it after the
;; fact, which itself costs time and may trigger extra redisplays.
;;
;; Most ideas are borrowed from doomemacs
;; (early-init.el, lisp/doom.el, lisp/doom-start.el). Each block has a
;; comment explaining *why* the optimization matters.
;;
;;; Code:


;; --------------------------------------------------------------
;;           GC: completely disable during startup
;; --------------------------------------------------------------
;; Emacs triggers GC when EITHER of these is true:
;;   (a) allocated bytes > gc-cons-threshold
;;   (b) allocated bytes > bytes-after-last-gc * (1 + gc-cons-percentage)
;;
;; Previously only `gc-cons-threshold' was raised in init.el, but the
;; default `gc-cons-percentage' of 0.1 was still kicking in, firing GCs
;; during startup anyway. Both need to be relaxed together for startup
;; GC to actually be suppressed.
;;
;; WARNING: these values MUST be restored to something reasonable once
;; startup is done, otherwise memory usage runs away. `gcmh' (configured
;; in init-basic.el) takes care of that on `emacs-startup-hook'.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

;; During startup we don't need to compare mtimes between .el and .elc
;; files. The default `load-prefer-newer' = t makes every `load' stat
;; both files. With hundreds of files loaded at startup, those stat
;; calls add up. `doom sync' (or, for us, normal byte/native recompile)
;; is responsible for keeping .elc fresh, not the loader.
(setq load-prefer-newer nil)


;; --------------------------------------------------------------
;;          file-name-handler-alist: empty during startup
;; --------------------------------------------------------------
;; This alist tells Emacs which handler to use for special paths
;; (.gz, /ssh:, /sudo:, ...). The problem: virtually every file-related
;; function (`load', `require', `expand-file-name', `file-exists-p', ...)
;; walks this list and runs each entry's regexp against the current
;; path on every call. During startup that happens thousands of times
;; and adds up to hundreds of milliseconds.
;;
;; Fix: clear the alist during startup (but keep `jka-compr-handler' if
;; any of Emacs's built-in libraries ship as .el.gz), then restore it
;; on `emacs-startup-hook'.
(let ((old-value (default-toplevel-value 'file-name-handler-alist)))
  (setq file-name-handler-alist
        ;; Heuristic: if the uncompressed `calc-loaddefs.el' is on the
        ;; load-path, this Emacs build ships built-ins uncompressed and
        ;; the alist can be cleared entirely. Otherwise we must keep
        ;; `jka-compr-handler' so .el.gz built-ins can still be loaded.
        ;; macOS Homebrew Emacs is uncompressed; most Linux distros
        ;; ship compressed built-ins.
        (if (locate-file-internal "calc-loaddefs.el" load-path)
            nil
          (list (rassq 'jka-compr-handler old-value))))
  ;; Restore the original value once startup finishes.
  ;; depth 101 = run after all ordinary hooks, so that if any package
  ;; added entries to the alist during startup, we preserve them via
  ;; `delete-dups' + `append' (merge, don't overwrite).
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist
                    (delete-dups (append file-name-handler-alist old-value))))
            101))


;; --------------------------------------------------------------
;;           Package.el: suppress auto-initialization
;; --------------------------------------------------------------
;; Emacs 27+ auto-calls `package-initialize' between early-init.el and
;; init.el. Our init.el calls it explicitly, so we don't want that extra
;; auto-init. Setting this to nil here saves one redundant initialization.
;;
;; This MUST be set in early-init.el -- setting it in init.el is too
;; late, package.el has already auto-initialized by that point.
(setq package-enable-at-startup nil)


;; --------------------------------------------------------------
;;        Frame implied resize: the macOS startup trap
;; --------------------------------------------------------------
;; Emacs's startup sequence looks like:
;;   1. Create the initial frame using the system default font
;;      (e.g. Menlo 12pt, 80x25)
;;   2. init.el changes the font to Sarasa Term SC 15pt
;;   3. Emacs RESIZES the entire frame in pixels to match the new font
;;
;; Step 3 can take up to a full second on macOS NS builds -- AppKit's
;; frame resize goes through a full layout/redraw cycle. Setting this
;; to t tells Emacs not to resize the outer frame when the font
;; changes; the internal character grid adapts instead. Visually
;; there's no difference.
(setq frame-inhibit-implied-resize t)

;; Allow resizing the frame in single-pixel increments rather than in
;; whole character-cell steps. Lets tiling/fractional-DPI window
;; managers (and macOS full-screen) fit Emacs exactly.
(setq frame-resize-pixelwise t)


;; --------------------------------------------------------------
;;           Startup screen / scratch buffer
;; --------------------------------------------------------------
;; `inhibit-startup-screen' only suppresses *displaying* the splash
;; screen, Emacs still builds it internally (rendering the logo,
;; preparing text, etc.). `initial-major-mode' is changed to
;; `fundamental-mode' because the default `lisp-interaction-mode'
;; loads a pile of elisp-mode machinery into the scratch buffer.
;; `initial-scratch-message' nil skips the default comment text.
;;
;; `inhibit-startup-echo-area-message' has an anti-abuse design: its
;; value must equal the current login name for the suppression to
;; trigger. This prevents users from blindly setting it to t and
;; missing important startup messages.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; The variables above control *display*, but the underlying functions
;; still get called. Override them to no-ops to skip the construction
;; work entirely (the startup screen involves image I/O). Belt and
;; suspenders.
(advice-add #'display-startup-screen :override #'ignore)
(advice-add #'display-startup-echo-area-message :override #'ignore)


;; --------------------------------------------------------------
;;       Disable menu bar / tool bar / scroll bar at the source
;; --------------------------------------------------------------
;; WRONG way (what used to be in init-basic.el): (tool-bar-mode -1)
;;   -> The initial frame is first built WITH a tool bar (which
;;      triggers `tool-bar-setup' to load tool bar icon resources),
;;      then the bar is torn down. All that loading work is wasted.
;;
;; RIGHT way: modify `default-frame-alist' BEFORE the frame is created
;; so the frame is built without these elements in the first place.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Also set the mode variables to nil, otherwise `M-x' sees a state
;; that doesn't match reality (user would be confused why tool-bar-mode
;; is t but no tool bar shows up).
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Even with `tool-bar-lines' = 0, Emacs's initialization still calls
;; `tool-bar-setup' to populate tool bar buttons -- which loads a bunch
;; of icon images. Override it to a no-op to skip that I/O entirely.
(advice-add #'tool-bar-setup :override #'ignore)

;; macOS special case:
;; On macOS the menu bar lives at the top of the screen (outside the
;; Emacs frame), so disabling it doesn't save any real estate. Worse,
;; disabling it turns Emacs's GUI frame into a "non-application window"
;; -- it won't capture focus on click, Dock behavior gets weird, etc.
;; So keep the menu bar on macOS GUI frames. TTY frames (Emacs in a
;; terminal) aren't affected and still get no menu bar.
(when (eq system-type 'darwin)
  ;; 'tty is a sentinel: treated as 0 on TTY frames, and rewritten to
  ;; 1 on GUI frames by the hook below.
  (setcdr (assq 'menu-bar-lines default-frame-alist) 'tty)
  (add-hook 'after-make-frame-functions
            (lambda (&optional frame)
              (when (eq (frame-parameter frame 'menu-bar-lines) 'tty)
                (set-frame-parameter frame 'menu-bar-lines
                                     (if (display-graphic-p frame) 1 0))))))


;; --------------------------------------------------------------
;;          Mode line: hide it during startup
;; --------------------------------------------------------------
;; During startup, various modes repeatedly push things into the mode
;; line and each change can trigger a redraw. Setting it to nil means
;; there is simply no mode line to draw during startup. `doom-modeline'
;; (configured in init-basic.el) builds the real one on `after-init'.
(setq-default mode-line-format nil)


;; --------------------------------------------------------------
;;              Misc frame / UI defaults
;; --------------------------------------------------------------
;; Silence the bell as early as possible so nothing during init can
;; beep or flash.
(setq ring-bell-function 'ignore)

;; Defaults read by `window-divider-mode' when it's enabled later.
;; Living here means the defaults are in place before any frame
;; configuration code runs.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)


;; --------------------------------------------------------------
;;        Runtime rendering / interaction optimizations
;; --------------------------------------------------------------
;; These mostly affect *usage* smoothness, not just startup.

;; `auto-mode-alist' (filename -> major mode mapping) is walked twice
;; by default: case-sensitive first, then case-insensitive. The second
;; pass is basically useless on Linux/macOS -- pure waste.
(setq auto-mode-case-fold nil)

;; Disable bidirectional text reordering. Emacs supports RTL scripts
;; like Arabic and Hebrew by analyzing direction and possibly
;; reordering characters on every rendered line. For Chinese+English
;; usage we don't need this at all.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
;; BPA = Bracket Pair Algorithm, used to decide bracket direction in
;; mixed-direction text. With forced LTR above, it's unnecessary.
(setq bidi-inhibit-bpa t)

;; Don't draw "hollow" cursors or region highlights in non-selected
;; windows. Saves redisplay work when you have splits open.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Skip precise fontification while scrolling fast over large files.
;; Highlighting may lag a beat behind, but scrolling itself is
;; noticeably smoother.
(setq fast-but-imprecise-scrolling t)

;; IMPORTANT for this config:
;; Emacs's font cache tries to "compact" (free unused font objects) on
;; GC. That compaction is extremely slow for fonts with large glyph
;; sets -- Nerd Fonts (6000+ icons), Symbols Nerd Font Mono, CJK fonts
;; -- and each GC can freeze for 100-500ms. This config uses Sarasa
;; Term SC + Symbols Nerd Font Mono + doom-modeline + nerd-icons, so
;; compaction would stutter the editor constantly.
;; Cost: the font cache is never released, costing a few extra MB of
;; memory. Absolutely worth it.
(setq inhibit-compacting-font-caches t)

;; When new input arrives, pause ongoing fontification. Reduces the
;; "sticky" feel during fast typing. Emacs 29+ variable.
(setq redisplay-skip-fontification-on-input t)


;; --------------------------------------------------------------
;;                   LSP / subprocess I/O
;; --------------------------------------------------------------
;; Default chunk size for reading subprocess output is only 4KB, which
;; is too small for LSP / eglot and causes frequent read() syscalls
;; and parser restarts. 3MB is the value recommended in lsp-mode's
;; performance docs. Moved here from init.el to keep all perf knobs
;; in one place.
(setq read-process-output-max (* 3 1024 1024))

;;; early-init.el ends here
