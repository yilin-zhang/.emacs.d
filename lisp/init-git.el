;;; init-git.el --- Configurations for git. -*- lexical-binding: t -*-

;;; Commentary:
;; Configure Git integrations including Magit, blamer, and diff-hl.

;;; Code:

;; Magit
(use-package magit
  :bind ("C-x g" . magit-status)
  :init
  ;; Set before Magit loads, so that the mode never turns on.
  (setq magit-auto-revert-mode nil)
  :custom
  (magit-diff-refine-hunk t)           ; word-level diff in the selected hunk
  ;; Don't save repo buffers, which would run save hooks and formatters.
  (magit-save-repository-buffers nil)
  (magit-revision-insert-related-refs nil) ; less clutter in commit buffers
  ;; Otherwise checkouts with the same directory name, such as A/src and
  ;; B/src, get mixed up.
  (magit-uniquify-buffer-names nil)
  :config
  ;; PERF: use an absolute path, so that macOS doesn't look git up on every
  ;; call.
  (setopt magit-git-executable
          (or (executable-find magit-git-executable) "git"))
  ;; Make URLs in process output clickable.
  (add-hook 'magit-process-mode-hook #'goto-address-mode)
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash")))

;; Commit message conventions, also for a `git commit' in a terminal.
;;
;; PERF: `global-git-commit-mode' would load most of magit at startup, so
;; load git-commit when a commit message file is visited.
(use-package git-commit
  :ensure nil
  :preface
  ;; A copy of `git-commit-filename-regexp', which needs git-commit loaded.
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
  :custom
  (git-commit-summary-max-length 50)
  (git-commit-style-convention-checks
   '(overlong-summary-line non-empty-second-line))
  :config
  (add-hook 'git-commit-mode-hook
            (lambda () (setopt-local fill-column 72))))

;; Enable smerge-mode in files with conflict markers.  Its keys are under
;; `C-c ^'.
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
  :config
  ;; The theme sets the colors, and this only overrides the size.
  (face-spec-set 'blamer-face '((t :height 140))))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         ;; Click a gutter bar to show the hunk.
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (dired-mode . diff-hl-dired-mode-unless-remote))
  :preface
  ;; ---------------------------------------------------------------
  ;; Gutter appearance
  ;; ---------------------------------------------------------------
  ;; Draw a thin bar in the left half of the fringe, replacing the default
  ;; full-width block.  Ported from doom's `ui/vc-gutter' module.
  (defun yilin/diff-hl-thin-bitmaps (&rest _)
    "Redefine `diff-hl-bmp-middle' as a thin bar as tall as a line."
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
           (w (min (or (frame-parameter
                        nil (intern (format "%s-fringe" diff-hl-side)))
                       diff-hl-bmp-max-width)
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

  ;; Otherwise TRAMP asks about the temp file every time the gutter updates.
  (defun yilin/diff-hl-silence-temp-file-prompts (fn &rest args)
    (let ((tramp-allow-unsafe-temporary-files t))
      (apply fn args)))

  ;; Reverting a hunk leaves point somewhere far from the hunk.
  (defun yilin/diff-hl-save-excursion (fn &rest args)
    (let ((pt (point)))
      (prog1 (apply fn args)
        (goto-char pt))))
  :custom
  (diff-hl-update-async t)
  ;; Better diff algorithm for what the gutter shows.
  (vc-git-diff-switches '("--histogram"))
  ;; Mark staged changes apart from unstaged ones.  The default highlighter
  ;; hides them.
  (diff-hl-show-staged-changes nil)
  (diff-hl-highlight-reference-function #'diff-hl-highlight-on-fringe)
  (diff-hl-global-modes '(not image-mode pdf-view-mode))
  (diff-hl-draw-borders nil)
  (diff-hl-fringe-bmp-function #'yilin/diff-hl-bmp-at-pos)
  :config
  (advice-add 'diff-hl-define-bitmaps :after #'yilin/diff-hl-thin-bitmaps)
  (yilin/diff-hl-thin-bitmaps)
  (advice-add 'diff-hl-diff-buffer-with-reference :around
              #'yilin/diff-hl-silence-temp-file-prompts)
  (advice-add 'diff-hl-revert-hunk :around #'yilin/diff-hl-save-excursion)
  (with-no-warnings
    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

(use-package diff-hl-refresh
  :load-path yilin/site-lisp-directory
  ;; Notice git commands run outside Emacs.
  :hook (after-init . diff-hl-refresh-mode)
  :config
  ;; Flydiff's git processes choke Emacs on macOS (doomemacs/core#8554), so
  ;; refresh on switching windows and on leaving meow's insert state.
  (diff-hl-refresh-on-select-mode)
  (with-eval-after-load 'meow
    (add-hook 'meow-insert-exit-hook #'diff-hl-refresh-maybe)))

;; Step through a file's history one revision at a time.
(use-package git-timemachine
  :bind ("C-c g t" . git-timemachine))

;; Open the current file or line on GitHub, GitLab, etc.
(use-package browse-at-remote
  :bind ("C-c g b" . browse-at-remote)
  :custom
  ;; Add line numbers only for a region, and link to the commit.
  (browse-at-remote-add-line-number-if-no-region-selected nil)
  (browse-at-remote-prefer-symbolic nil))

(provide 'init-git)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-git.el ends here
