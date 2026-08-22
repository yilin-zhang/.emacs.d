;;; init-git.el --- Configurations for git. -*- lexical-binding: t -*-

;;; Commentary:
;; Configure Git integrations including Magit, blamer, and diff-hl.

;;; Code:

;; Magit
(use-package magit
  :commands (transient-insert-suffix magit-status)
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-auto-revert-mode nil)
  :config
  (setq magit-diff-refine-hunk t           ; word-level diff in the selected hunk
        magit-save-repository-buffers nil  ; don't autosave repo buffers (avoids
                                           ; surprise save-hooks / formatters)
        magit-revision-insert-related-refs nil) ; less clutter in commit buffers
  ;; Turn URLs in process output into clickable buttons.
  (add-hook 'magit-process-mode-hook #'goto-address-mode)
  ;; A couple of switches common enough to want one keystroke away.
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash")))

;; Enforce good commit-message conventions: 50-char summary, blank
;; second line, 72-char body wrap. This also catches COMMIT_EDITMSG
;; opened from a terminal `git commit' via emacsclient, not just
;; commits started from magit.
;;
;; PERF: in magit 4.x, git-commit.el `require's the magit core
;; (magit-git, magit-mode, magit-process, transient, with-editor) at
;; load time, so enabling `global-git-commit-mode' on `after-init'
;; would load most of magit at every startup. Instead, match filenames
;; ourselves on `find-file' and only load git-commit when a commit
;; message file is actually visited.
(use-package git-commit
  :ensure nil
  :preface
  ;; Mirrors `git-commit-filename-regexp', inlined so the check runs
  ;; without loading git-commit.
  (defconst yilin/git-commit-filename-regexp "/\\(\
\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'")
  (defun yilin/maybe-git-commit-setup ()
    (when (and buffer-file-name
               (string-match-p yilin/git-commit-filename-regexp
                               buffer-file-name))
      (require 'git-commit)
      (git-commit-setup-check-buffer)))
  :hook (find-file . yilin/maybe-git-commit-setup)
  :config
  (setq git-commit-summary-max-length 50
        git-commit-style-convention-checks
        '(overlong-summary-line non-empty-second-line))
  (add-hook 'git-commit-mode-hook
            (lambda () (setq-local fill-column 72))))

;; Auto-enable smerge-mode when opening a file that already contains
;; conflict markers, so merge conflicts come with resolution UI (default
;; keymap under `C-c ^').
(use-package smerge-mode
  :ensure nil
  :preface
  (defun yilin/maybe-enable-smerge ()
    (unless (bound-and-true-p so-long-detected-p)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode 1)))))
  :hook (find-file . yilin/maybe-enable-smerge))

(use-package blamer
  :commands blamer-mode
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground ,(face-foreground 'warning)
                   :background unspecified
                   :height 140
                   :italic t))))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         ;; Ships with diff-hl (autoloaded from diff-hl-dired).
         (dired-mode . diff-hl-dired-mode-unless-remote))
  :preface
  (defvar diff-hl-flydiff-delay)        ; lives in diff-hl-flydiff.el
  ;; ---------------------------------------------------------------
  ;; Gutter appearance
  ;; ---------------------------------------------------------------
  ;; The stock bitmap is a full-fringe block with a border, which is
  ;; heavy and monopolizes the fringe. Redefine it as a solid bar taking
  ;; up only the left half of the fringe, so the other half stays
  ;; available to whoever else wants it (magit, flyspell, ...). Ported
  ;; from doom's `ui/vc-gutter' +pretty flag.
  (defun yilin/diff-hl-thin-bitmaps (&rest _)
    "Redefine `diff-hl-bmp-middle' as a thin, borderless bar.
Height tracks `frame-char-height', the buffer's text scale and
`line-spacing' so the bar still spans a full line when either changes."
    (let* ((scale (if (and (boundp 'text-scale-mode-amount)
                           (numberp text-scale-mode-amount))
                      (expt text-scale-mode-step text-scale-mode-amount)
                    1))
           (spacing (or (and (display-graphic-p) (default-value 'line-spacing)) 0))
           (total-spacing (pcase spacing
                            ((pred numberp) spacing)
                            (`(,above . ,below) (+ above below))))
           (h (+ (ceiling (* (frame-char-height) scale))
                 (if (floatp total-spacing)
                     (truncate (* (frame-char-height) total-spacing))
                   total-spacing)))
           (w (min (frame-parameter nil (intern (format "%s-fringe" diff-hl-side)))
                   diff-hl-bmp-max-width))
           (_ (if (zerop w) (setq w diff-hl-bmp-max-width))))
      (define-fringe-bitmap 'diff-hl-bmp-middle
        (make-vector
         h (string-to-number (let ((half-w (1- (/ w 2))))
                               (concat (make-string half-w ?1)
                                       (make-string (- w half-w) ?0)))
                             2))
        nil nil 'center)))

  (defun yilin/diff-hl-bmp-at-pos (type _pos)
    (if (eq type 'delete) 'diff-hl-bmp-delete 'diff-hl-bmp-middle))

  ;; doom-gruvbox gives the diff-hl faces backgrounds we don't want (the
  ;; bar should be a colored stroke, not a highlighted cell). Re-derive
  ;; the colors from the semantic faces. Must re-run after every theme
  ;; load, otherwise the new theme wins.
  (defun yilin/diff-hl-restyle-faces (&rest _)
    (pcase-dolist (`(,face . ,source) '((diff-hl-change . warning)
                                        (diff-hl-insert . success)
                                        (diff-hl-delete . error)))
      (set-face-attribute face nil
                          :background 'unspecified
                          :foreground (face-foreground source))))

  ;; ---------------------------------------------------------------
  ;; Refreshing the gutter
  ;; ---------------------------------------------------------------
  ;; flydiff is off on macOS (see below), which would leave the gutter
  ;; only updating on save. Compensate the way doom does: refresh when
  ;; you switch windows or leave meow's insert state, debounced against
  ;; the file's cached VC state so we don't spawn a git process per
  ;; keystroke.
  (defvar-local yilin/diff-hl--last-state nil)

  (defun yilin/diff-hl-update-maybe ()
    "Refresh the diff-hl gutter unless nothing can have changed."
    (when-let* (((or (bound-and-true-p diff-hl-mode)
                     (bound-and-true-p diff-hl-dir-mode)))
                ;; Refreshing over TRAMP is far too slow to do on a
                ;; window switch.
                ((not (file-remote-p default-directory)))
                (file (buffer-file-name (buffer-base-buffer)))
                ((not (equal (cons (point) yilin/diff-hl--last-state)
                             (setq yilin/diff-hl--last-state
                                   (cons (point)
                                         (copy-sequence
                                          (symbol-plist
                                           (intern (expand-file-name file)
                                                   vc-file-prop-obarray)))))))))
      (ignore (diff-hl-update))))

  (defun yilin/diff-hl-update-on-switch (&optional frame-or-window)
    "Refresh the gutter of the newly selected window.
`window-selection-change-functions' passes the frame for the global hook
value and the window for a window-local one, and neither guarantees the
matching buffer is current, so resolve it explicitly."
    (let ((win (cond ((windowp frame-or-window) frame-or-window)
                     ((framep frame-or-window)
                      (frame-selected-window frame-or-window))
                     (t (selected-window)))))
      (when (window-live-p win)
        (with-current-buffer (window-buffer win)
          (yilin/diff-hl-update-maybe)))))

  ;; diff-hl builds its temp file paths through the auto-save mechanism,
  ;; which makes TRAMP buffers prompt "auto-save file in local temp dir,
  ;; continue?" every time the gutter updates.
  (defun yilin/diff-hl-silence-temp-file-prompts (fn &rest args)
    (let ((tramp-allow-unsafe-temporary-files t))
      (apply fn args)))

  ;; Reverting a hunk leaves point somewhere far from the hunk.
  (defun yilin/diff-hl-save-excursion (fn &rest args)
    (let ((pt (point)))
      (prog1 (apply fn args)
        (goto-char pt))))
  :config
  (setq diff-hl-update-async t
        ;; Better diff algorithm for what the gutter shows.
        vc-git-diff-switches '("--histogram")
        ;; Realtime gutter feedback after staging / unstaging hunks.
        diff-hl-show-staged-changes nil
        ;; Nothing to diff in a rendered image or PDF.
        diff-hl-global-modes '(not image-mode pdf-view-mode)
        ;; The thin bar is its own border.
        diff-hl-draw-borders nil
        diff-hl-fringe-bmp-function #'yilin/diff-hl-bmp-at-pos)
  (advice-add 'diff-hl-define-bitmaps :after #'yilin/diff-hl-thin-bitmaps)
  (yilin/diff-hl-thin-bitmaps)
  ;; On-the-fly gutter updates -- but NOT on macOS, where newer versions
  ;; choke on the many short-lived git subprocesses flydiff spawns (see
  ;; doomemacs/core#8554); there we fall back to updating on save plus
  ;; the switch/insert-exit hooks below.
  (if (eq system-type 'darwin)
      (progn
        (add-hook 'window-selection-change-functions
                  #'yilin/diff-hl-update-on-switch)
        (with-eval-after-load 'meow
          (add-hook 'meow-insert-exit-hook #'yilin/diff-hl-update-maybe)))
    ;; Slightly more conservative than the 0.3 default.
    (setq diff-hl-flydiff-delay 0.5)
    (add-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode))
  (yilin/diff-hl-restyle-faces)
  (add-hook 'enable-theme-functions #'yilin/diff-hl-restyle-faces)
  (advice-add 'diff-hl-diff-buffer-with-reference :around
              #'yilin/diff-hl-silence-temp-file-prompts)
  (advice-add 'diff-hl-revert-hunk :around #'yilin/diff-hl-save-excursion)
  (with-no-warnings
    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

;; Step through a file's history one revision at a time.
(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine))

;; Open the current file / line on the remote forge (GitHub, GitLab, ...).
(use-package browse-at-remote
  :bind ("C-c g b" . browse-at-remote)
  :config
  ;; Only add a line number when a region is selected, and produce
  ;; permalinks (commit hash) rather than branch-relative URLs.
  (setq browse-at-remote-add-line-number-if-no-region-selected nil
        browse-at-remote-prefer-symbolic nil))

(provide 'init-git)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-git.el ends here
