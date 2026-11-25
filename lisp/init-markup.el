;;; init-markup.el --- Configurations for markup languages. -*- lexical-binding: t -*-

;;; Commentary:
;; Configure editing support for LaTeX, Markdown, YAML, CSV, and Mermaid.

;;; Code:

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
  :mode ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-command "multimarkdown")
  (markdown-fontify-code-blocks-natively t)
  (markdown-fontify-whole-heading-line t)
  (markdown-open-command "open")
  ;; Make exported HTML look like GitHub, with code highlighting and math.
  (markdown-content-type "application/xhtml+xml")
  (markdown-css-paths
   '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"))
  ;; The code theme follows the system's light or dark setting.  %1$s is the
  ;; highlight.js URL.
  (markdown-xhtml-header-content
   (format "\
<meta name='viewport' content='width=device-width, initial-scale=1'/>
<style>
  body { box-sizing: border-box; max-width: 740px; width: 100%%;
         margin: 40px auto; padding: 0 10px; }
</style>
<link rel='stylesheet' media='(prefers-color-scheme: light)'
      href='%1$s/styles/github.min.css'/>
<link rel='stylesheet' media='(prefers-color-scheme: dark)'
      href='%1$s/styles/github-dark.min.css'/>
<script id='MathJax-script' async
        src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>
<script src='%1$s/highlight.min.js'></script>
<script>
  document.addEventListener('DOMContentLoaded', () => {
    document.body.classList.add('markdown-body');
    for (const code of document.querySelectorAll('pre > code')) {
      const lang = code.parentElement.lang;
      if (lang) code.classList.add(lang);
      hljs.highlightElement(code);
    }
  });
</script>"
           "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build"))
  :config
  ;; Highlight Rust and Python code blocks with tree-sitter.
  (add-to-list 'markdown-code-lang-modes '("rust" . rust-ts-mode))
  (add-to-list 'markdown-code-lang-modes '("python" . python-ts-mode))

  ;; PERF: don't run mode hooks to fontify code blocks (from doom).
  (advice-add 'markdown-fontify-code-block-natively :around
              (lambda (fn &rest args) (delay-mode-hooks (apply fn args)))))

;; Generate a table of contents.
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

;; --------------------------------------------------------------
;;                      Typst Mode Configuration
;; --------------------------------------------------------------
(use-package typst-ts-mode
  :init
  ;; Some Typst projects bundle fonts and set this option in .dir-locals.el.
  ;; Only this value is safe, because the option goes to a shell command.
  (put 'typst-ts-compile-options 'safe-local-variable
       (lambda (value)
         (equal value '("--font-path" "fonts")))))

(provide 'init-markup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markup.el ends here
