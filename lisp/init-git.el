;; init-git.el --- Configurations for git. -*- lexical-binding: t -*-

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
;; second line, 72-char body wrap. `global-git-commit-mode' also catches
;; COMMIT_EDITMSG opened from a terminal `git commit' via emacsclient,
;; not just commits started from magit. `git-commit' ships inside magit.
(use-package git-commit
  :ensure nil
  :hook (after-init . global-git-commit-mode)
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
  :preface
  (defun yilin/diff-hl-dired-mode-unless-remote ()
    (unless (file-remote-p default-directory)
      (diff-hl-dired-mode)))
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . yilin/diff-hl-dired-mode-unless-remote))
  :config
  (setq diff-hl-update-async t
        ;; Better diff algorithm for what the gutter shows.
        vc-git-diff-switches '("--histogram")
        ;; Realtime gutter feedback after staging / unstaging hunks.
        diff-hl-show-staged-changes nil)
  ;; On-the-fly gutter updates -- but NOT on macOS, where newer versions
  ;; choke on the many short-lived git subprocesses flydiff spawns (see
  ;; doom #8554); there we fall back to updating on save.
  (unless (eq system-type 'darwin)
    (add-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode))
  (set-face-attribute 'diff-hl-change nil
                      :background 'unspecified
                      :foreground (face-foreground 'warning))
  (set-face-attribute 'diff-hl-insert nil
                      :background 'unspecified
                      :foreground (face-foreground 'success))
  (set-face-attribute 'diff-hl-delete nil
                      :background 'unspecified
                      :foreground (face-foreground 'error))
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
