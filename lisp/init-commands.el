;;; init-commands.el --- Personal interactive commands -*- lexical-binding: t -*-

;;; Commentary:
;; Personal interactive commands and their keybindings.  Loaded as a
;; regular config module from init.el.

;;; Code:

;; --------------------------------------------------------------
;;                           Editing
;; --------------------------------------------------------------

;; **************************************************************
;; Unfill paragraph (the reversed operation to auto-fill)
;; `https://www.emacswiki.org/emacs/UnfillParagraph'
;; **************************************************************
(defun yilin/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(declare-function LaTeX-narrow-to-environment "latex" (&optional count))
(defun yilin/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (declare (interactive-only t))
  (interactive "P")
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(defun yilin/delete-minibuffer-directory ()
  "Delete the directory name before the last slash in the minibuffer."
  (interactive)
  (let* ((end (point))
         (beg (save-excursion
                (when (eq ?/ (char-before))
                  (backward-char))
                (if (search-backward "/" nil t)
                    (1+ (point))
                  (backward-word)
                  (point)))))
    (delete-region beg end)))

(defun yilin/insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y%m%d%H%M%S")))

(defun yilin/remove-empty-lines ()
  "Remove all empty lines (containing only whitespace) in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (flush-lines "^[ \t]*$")))

(defun yilin/-region-text ()
  "Return the active region's text with whitespace trimmed, or nil."
  (when (use-region-p)
    (string-trim (buffer-substring-no-properties
                  (region-beginning) (region-end)))))

(defconst yilin/greek-math-symbol-map
  '(;; --- Greek letters (lowercase) ---
    ("alpha" . "α") ("beta" . "β") ("gamma" . "γ") ("delta" . "δ")
    ("epsilon" . "ε") ("zeta" . "ζ") ("eta" . "η") ("theta" . "θ")
    ("iota" . "ι") ("kappa" . "κ") ("lambda" . "λ") ("mu" . "μ")
    ("nu" . "ν") ("xi" . "ξ") ("omicron" . "ο") ("pi" . "π")
    ("rho" . "ρ") ("sigma" . "σ") ("tau" . "τ") ("upsilon" . "υ")
    ("phi" . "φ") ("chi" . "χ") ("psi" . "ψ") ("omega" . "ω")

    ;; --- Greek letters (uppercase) ---
    ("Alpha" . "Α") ("Beta" . "Β") ("Gamma" . "Γ") ("Delta" . "Δ")
    ("Epsilon" . "Ε") ("Zeta" . "Ζ") ("Eta" . "Η") ("Theta" . "Θ")
    ("Iota" . "Ι") ("Kappa" . "Κ") ("Lambda" . "Λ") ("Mu" . "Μ")
    ("Nu" . "Ν") ("Xi" . "Ξ") ("Omicron" . "Ο") ("Pi" . "Π")
    ("Rho" . "Ρ") ("Sigma" . "Σ") ("Tau" . "Τ") ("Upsilon" . "Υ")
    ("Phi" . "Φ") ("Chi" . "Χ") ("Psi" . "Ψ") ("Omega" . "Ω")

    ;; --- Logic ---
    ("forall" . "∀") ("exists" . "∃") ("nexists" . "∄")
    ("neg" . "¬") ("lnot" . "¬")
    ("land" . "∧") ("lor" . "∨")
    ("implies" . "⇒") ("iff" . "⇔")

    ;; --- Sets ---
    ("in" . "∈") ("notin" . "∉")
    ("subset" . "⊂") ("subseteq" . "⊆") ("nsubseteq" . "⊈")
    ("supset" . "⊃") ("supseteq" . "⊇") ("nsupseteq" . "⊉")
    ("emptyset" . "∅")
    ("cap" . "∩") ("cup" . "∪")
    ("propto" . "∝")

    ;; --- Relations ---
    ("=" . "=") ("!=" . "≠") ("equiv" . "≡") ("neq" . "≠")
    ("approx" . "≈") ("sim" . "∼") ("cong" . "≅")
    ("<=" . "≤") (">=" . "≥") ("<<" . "≪") (">>" . "≫")

    ;; --- Arithmetic ---
    ("times" . "×") ("cdot" . "·") ("ast" . "∗")
    ("pm" . "±") ("mp" . "∓")
    ("div" . "÷")
    ("sqrt" . "√") ("cbrt" . "∛") ("qdrt" . "∜")

    ;; --- Calculus & Algebra ---
    ("int" . "∫") ("iint" . "∬") ("iiint" . "∭")
    ("oint" . "∮") ("oiint" . "∯")
    ("sum" . "∑") ("prod" . "∏") ("lim" . "lim") ;; leave lim as text
    ("infty" . "∞") ("infinity" . "∞")
    ("nabla" . "∇") ("partial" . "∂")

    ;; --- Arrows ---
    ("->" . "→") ("=>" . "⇒") ("-->" . "⟶")
    ("<-" . "←") ("<--" . "⟵")
    ("<->" . "↔") ("<=>" . "⇔")
    ("uparrow" . "↑") ("downarrow" . "↓")
    ("Uparrow" . "⇑") ("Downarrow" . "⇓")
    ("leftrightarrow" . "↔") ("Rightarrow" . "⇒")
    ("Leftarrow" . "⇐") ("Leftrightarrow" . "⇔")

    ;; --- Geometry ---
    ("angle" . "∠") ("measuredangle" . "∡") ("perp" . "⊥") ("parallel" . "∥")

    ;; --- Miscellaneous ---
    ("degree" . "°") ("prime" . "′")
    ("ell" . "ℓ") ("hbar" . "ℏ") ("Re" . "ℜ") ("Im" . "ℑ")
    ("aleph" . "ℵ")
    ("top" . "⊤") ("bot" . "⊥"))
  "Alist mapping LaTeX-like names to Greek/math unicode symbols.
Used by `yilin/insert-greek-or-math-symbol'.")

(defun yilin/insert-greek-or-math-symbol ()
  "Replace region with corresponding Greek or math symbol, or prompt for one.
If a region is active, trim whitespace around it and try to convert
its contents into the corresponding Greek or math symbol. If successful,
replace the region. If not, or if no region is active, prompt the user."
  (interactive)
  (let* ((text (yilin/-region-text))
         (replacement (assoc-default text yilin/greek-math-symbol-map)))
    (if (and text replacement)
        ;; Replace region
        (progn
          (delete-region (region-beginning) (region-end))
          (insert replacement))
      ;; Prompt for input
      (let* ((name (completing-read "Greek letter: "
                                    (mapcar #'car yilin/greek-math-symbol-map)
                                    nil t))
             (symbol (assoc-default name yilin/greek-math-symbol-map)))
        (when symbol
          (when (use-region-p)
            (delete-region (region-beginning) (region-end)))
          (insert symbol))))))

(defun yilin/insert-accented (accent char)
  "Insert CHAR with ACCENT. Uses combining marks when needed.
ACCENT choices: ^ ` ' ~ \" ."
  (interactive
   (list
    (read-char-choice "Accent (^ ` ' ~ \"): " '(?^ ?` ?' ?~ ?\" ?.))
    (read-char "Character: ")))
  (let* ((combining-map
          '((?^ . ?\u0302)   ;; COMBINING CIRCUMFLEX
            (?` . ?\u0300)   ;; COMBINING GRAVE
            (?' . ?\u0301)   ;; COMBINING ACUTE
            (?~ . ?\u0303)   ;; COMBINING TILDE
            (?\" . ?\u0308)  ;; COMBINING DIAERESIS
            (?. . ?\u0307))) ;; COMBINING DOT ABOVE
         (combining (cdr (assoc accent combining-map))))
    (insert (string char combining))))

;; **************************************************************
;; Quote lines
;; `http://xahlee.info/emacs/emacs/emacs_quote_lines.html'
;; **************************************************************
(defun yilin/quote-lines ()
  "Wrap each line in the current block with quotes and a separator.
If a region is active, act on the region; otherwise act on the text
block delimited by blank lines.

For example,

  cat
  dog
  cow

becomes

  \"cat\",
  \"dog\",
  \"cow\",

or

  (cat)
  (dog)
  (cow)

If the delimiter is a left bracket, the closing delimiter is the
matching right bracket.

URL `http://ergoemacs.org/emacs/emacs_quote_lines.html'
Version 2017-01-08"
  (interactive)
  (let* (
         $p1
         $p2
         ($quoteToUse
          (read-string
           "Quote to use:" "\"" nil
           '(
             ""
             "\""
             "'"
             "("
             "{"
             "["
             )))
         ($separator
          (read-string
           "line separator:" "," nil
           '(
             ""
             ","
             ";"
             )))
         ($beginQuote $quoteToUse)
         ($endQuote
          ;; if begin quote is a bracket, set end quote to the matching one. else, same as begin quote
          (let (($syntableValue (aref (syntax-table) (string-to-char $beginQuote))))
            (if (eq (car $syntableValue) 4) ; ; syntax table, code 4 is open paren
                (char-to-string (cdr $syntableValue))
              $quoteToUse
              ))))
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (progn
        (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "NOERROR")
        (skip-chars-backward " \t\n")
        (setq $p2 (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (skip-chars-forward "\t ")
        (insert $beginQuote)
        (goto-char (point-max))
        (insert $endQuote)
        (goto-char (point-min))
        (while (re-search-forward "\n\\([\t ]*\\)" nil "NOERROR")
          (replace-match
           (concat $endQuote $separator (concat "\n" (match-string 1)) $beginQuote) "FIXEDCASE" "LITERAL"))
        ;;
        ))))

;; --------------------------------------------------------------
;;              Open in finder and terminal (Mac)
;; --------------------------------------------------------------

(defun yilin/expand-and-quote-default-directory ()
  (shell-quote-argument (expand-file-name default-directory)))

(defun yilin/macos-terminal-app ()
  "Return the preferred terminal app available on macOS."
  (cond
   ((file-directory-p "/Applications/Ghostty.app") "Ghostty")
   ((file-directory-p "/Applications/iTerm.app") "iTerm")
   ((file-directory-p "/System/Applications/Utilities/Terminal.app") "Terminal")
   ((file-directory-p "/Applications/Utilities/Terminal.app") "Terminal")
   (t (user-error "No supported terminal app found"))))

(defun yilin/open-with-terminal ()
  "Open the current dir in a terminal tab when possible.

Prefer Ghostty, then iTerm, then macOS Terminal."
  (interactive)
  (call-process "open" nil 0 nil
                "-a" (yilin/macos-terminal-app)
                (expand-file-name default-directory)))

(defun yilin/open-with-finder-or-default-app ()
  "Open the current dir with Finder or open the current file with a default app"
  (interactive)
  (let ((dired-file (condition-case nil
                        (dired-get-filename)
                      (error nil))))
    (if dired-file
        (shell-command (concat "open " (shell-quote-argument dired-file)))
      (shell-command (concat "open " (yilin/expand-and-quote-default-directory))))))

;; --------------------------------------------------------------
;;                       File-path helpers
;; --------------------------------------------------------------

(defun yilin/-copy-file-path ()
  "Return the file path of the current buffer or file under cursor.
Returns nil if the buffer is not visiting a file and no file is under cursor."
  (cond
   ;; If buffer is visiting a file, return its path
   ((buffer-file-name) (buffer-file-name))
   ;; If in dired mode, return the full path of file under cursor
   ((eq major-mode 'dired-mode)
    (ignore-errors (dired-get-filename)))
   ;; Otherwise return nil
   (t nil)))

(defun yilin/copy-file-path ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (let ((filename (yilin/-copy-file-path)))
    (when filename
      (kill-new filename)
      (message "Copied buffer file path '%s' to the clipboard." filename))))

(defun yilin/copy-file-name ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (let ((path (yilin/-copy-file-path)))
    (if path
        (let ((filename (file-name-base path)))
          (kill-new filename)
          (message "Copied buffer file name '%s' to the clipboard." filename))
      (message "No file name available to copy."))))

;; --------------------------------------------------------------
;;                        Web lookups
;; --------------------------------------------------------------

(defun yilin/lookup-thesaurus (&optional arg)
  "Look up the word on an online thesaurus.
If a region is active, use the text in that region (whitespaces stripped).
If no region is active, use the word at point.
If no word is at point, prompt for a word, using prefix ARG as default
if provided."
  (interactive "P")
  (let ((word (or (yilin/-region-text)
                  (thing-at-point 'word t)
                  (read-string "Word for thesaurus: "
                               (when arg (format "%s" arg))))))
    (browse-url (format "https://www.merriam-webster.com/thesaurus/%s" word))))

(defun yilin/arxiv-get-paper-id ()
  "Get arXiv paper ID from user input or selected region.
Returns the paper ID as a trimmed string."
  (let* ((region-text (yilin/-region-text))
         (looks-like-arxiv-id (when region-text
                                (string-match-p "^\\([0-9]+\\.[0-9]+\\|[a-z-]+\\.[A-Z]+/[0-9]+\\)$" region-text)))
         (prompt (if looks-like-arxiv-id
                     (format "Enter arXiv paper ID (%s): " region-text)
                   "Enter arXiv paper ID: "))
         (user-input (read-string prompt)))
    (cond
     ;; If user just pressed enter and we have a valid region ID, use it
     ((and looks-like-arxiv-id (string-empty-p user-input))
      (when (use-region-p) (delete-region (region-beginning) (region-end)))
      region-text)
     ;; If user entered something, use that
     ((not (string-empty-p user-input))
      (when (use-region-p) (delete-region (region-beginning) (region-end)))
      (string-trim user-input))
     ;; Fallback: use whatever the user typed
     (t (string-trim user-input)))))

(defun yilin/arxiv-insert-org-link (paper-id)
  "Insert an org-mode link for an arXiv paper given its ID.
If called interactively, prompts for paper ID or uses selected text.
PAPER-ID should be in format like `2301.07041' or `math.GT/0309136'."
  (interactive (list (yilin/arxiv-get-paper-id)))
  (require 'url) ; simple lazy load
  (let* ((url (format "https://arxiv.org/abs/%s" paper-id))
         (title (condition-case nil
                    (let ((buffer (url-retrieve-synchronously url t nil 10)))
                      (when buffer
                        (unwind-protect
                            (with-current-buffer buffer
                              (goto-char (point-min))
                              (when (re-search-forward "<title>\\[.*?\\]\\s-*\\(.*?\\)</title>" nil t)
                                (string-trim (match-string 1))))
                          (kill-buffer buffer))))
                  (error nil))))
    (insert (format "[[%s][%s]]" url (or title (format "arXiv:%s" paper-id))))
    (message "Inserted link for %s" (or title paper-id))))

(defun yilin/arxiv-open-paper (paper-id)
  "Open an arXiv paper in the default browser.
If called interactively, prompts for paper ID or uses selected text.
PAPER-ID should be in format like `2301.07041' or `math.GT/0309136'."
  (interactive (list (yilin/arxiv-get-paper-id)))
  (browse-url (format "https://arxiv.org/abs/%s" paper-id)))

(defun yilin/org-insert-link-with-html-title (url)
  "Asynchronously fetch URL and insert an Org link with the page title at point."
  (interactive "sEnter URL: ")
  ;; Capture the buffer and point where we should insert later
  (let ((target-buf (current-buffer))
        (target-pos (point)))
    (url-retrieve
     url
     #'yilin/org--insert-link-callback
     (list url target-buf target-pos))))

(defun yilin/org--insert-link-callback (status url target-buf target-pos)
  "Callback for `yilin/org-insert-link-with-title-async'.
STATUS is the retrieval status. URL is the original URL.
TARGET-BUF and TARGET-POS are where to insert the link."
  (require 'url)
  (require 'dom)
  (if (plist-get status :error)
      (message "Error fetching URL: %s" (plist-get status :error))
    (goto-char (point-min))
    ;; Skip HTTP headers
    (re-search-forward "\n\n" nil 'move)
    (let* ((dom (libxml-parse-html-region (point) (point-max)))
           (title-node (car (dom-by-tag dom 'title)))
           (title (when title-node (string-trim (dom-text title-node)))))
      ;; Clean up this temporary buffer
      (kill-buffer (current-buffer))
      ;; Insert into the original buffer at original position
      (when (buffer-live-p target-buf)
        (with-current-buffer target-buf
          (save-excursion
            (goto-char target-pos)
            (insert (if title
                        (format "[[%s][%s]]" url title)
                      (format "[[%s]]" url))))
          (message "Inserted link for %s" url))))))

;; --------------------------------------------------------------
;;                         Keybindings
;; --------------------------------------------------------------

(bind-keys
 ("M-Q" . yilin/unfill-paragraph)
 :map ctl-x-map
 ("n" . yilin/narrow-or-widen-dwim)
 :map minibuffer-local-map
 ("M-<backspace>" . yilin/delete-minibuffer-directory))

(with-eval-after-load 'meow
  (meow-leader-define-key
   '("t" . yilin/open-with-terminal)
   '("e" . yilin/open-with-finder-or-default-app)))

(provide 'init-commands)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-commands.el ends here
