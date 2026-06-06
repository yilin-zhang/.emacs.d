;; init-markup.el --- Configurations for markup languages. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                     LaTeX Mode Configurations
;; --------------------------------------------------------------
(use-package tex
  :ensure auctex
  :config (set-default 'preview-scale-function 2.0))

(use-package cdlatex
  :hook
  (LaTeX-mode . cdlatex-mode)
  (LaTeX-mode . auto-fill-mode)
  (LaTeX-mode . outline-minor-mode))

;; --------------------------------------------------------------
;;                     Markdown Mode Configurations
;; --------------------------------------------------------------
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown")
  (markdown-fontify-code-blocks-natively t)
  ;; Highlight the whole heading line (background spans the line).
  (markdown-fontify-whole-heading-line t)
  ;; Let `markdown-open' open exports in the macOS default app.
  (markdown-open-command "open")
  :config
  ;; Fontify rust/python code blocks with the tree-sitter modes, so they
  ;; pick up the same (level 4) highlighting as real source buffers.
  (add-to-list 'markdown-code-lang-modes '("rust" . rust-ts-mode))
  (add-to-list 'markdown-code-lang-modes '("python" . python-ts-mode))

  ;; PERF: native code-block fontification calls a major mode without
  ;; inhibiting its hooks; suppress them to speed it up (from doom).
  (advice-add 'markdown-fontify-code-block-natively :around
              (lambda (fn &rest args) (delay-mode-hooks (apply fn args))))

  ;; GitHub-flavored HTML for `markdown-export' / `markdown-preview':
  ;; github-markdown-css for layout, highlight.js for code blocks, MathJax
  ;; for math. (Borrowed from doom.)
  (setq markdown-content-type "application/xhtml+xml"
        markdown-css-paths
        '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
          "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
        markdown-xhtml-header-content
        (concat "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
                "<style> body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; } </style>"
                "<script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>"
                "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
                "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>")))

;; Generate / refresh a table of contents in markdown buffers.
(use-package markdown-toc
  :after markdown-mode)

;; --------------------------------------------------------------
;;                     Mermaid Mode Configurations
;; --------------------------------------------------------------
(use-package mermaid-mode)

;; --------------------------------------------------------------
;;                     Data / Config
;; --------------------------------------------------------------
(use-package yaml-mode)

(use-package csv-mode)

(provide 'init-markup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markup.el ends here
