;; init-edit.el --- Configurations for a better editing experience. -*- lexical-binding: t -*-

;; --------------------------------------------------------------
;;                      Template and Spellchecker
;; --------------------------------------------------------------

(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode)
  :init
  (yas-global-mode 1))

;; On-the-fly spell checker
(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
	 (prog-mode . flyspell-prog-mode)
	 (flyspell-mode . (lambda ()
			    (dolist (key '("C-;" "C-," "C-."))
			      (unbind-key key flyspell-mode-map)))))
  :init
  (setq flyspell-issue-message-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

;; --------------------------------------------------------------
;;                      Parentheses and Region
;; --------------------------------------------------------------
;; enable show-paren-mode for emacs-lisp-mode
(show-paren-mode t)

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; rainbow parentheses
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; --------------------------------------------------------------
;;               Whitespace, Indentation and Delete
;; --------------------------------------------------------------
;; Visualize TAB, (HARD) SPACE, NEWLINE
(use-package whitespace
  :ensure nil
  :diminish
  :hook ((prog-mode outline-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-line-column fill-column) ;; limit line length
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  (setq whitespace-style '(face
			   trailing space-before-tab
			   indentation empty space-after-tab)))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :ensure t
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
	 ;; FIXME: Disable in big files due to the performance issues
	 ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
	 (find-file . (lambda ()
			(if (> (buffer-size) (* 3000 80))
			    (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode robot-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (or (derived-mode-p 'c-mode)
	     (derived-mode-p 'c++-mode)
	     (derived-mode-p 'csharp-mode)
	     (derived-mode-p 'java-mode)
	     (derived-mode-p 'go-mode)
	     (derived-mode-p 'swift-mode))
	 (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
			     (thing-at-point 'line))))))

;; highlight indentations
(use-package highlight-indent-guides
  :ensure t
  :diminish
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive t)
  ;; Disable `highlight-indet-guides-mode' in `swiper'
  ;; https://github.com/DarthFennec/highlight-indent-guides/issues/40
  (with-eval-after-load 'ivy
    (defadvice ivy-cleanup-string (after my-ivy-cleanup-hig activate)
      (let ((pos 0) (next 0) (limit (length str)) (prop 'highlight-indent-guides-prop))
	(while (and pos next)
	  (setq next (text-property-not-all pos limit prop nil str))
	  (when next
	    (setq pos (text-property-any next limit prop nil str))
	    (ignore-errors
	      (remove-text-properties next pos '(display nil face nil) str))))))))

;; Note that this package has conflict with smartparens.
;; The solution is written inside smartparens' configurations.
(use-package hungry-delete
  :ensure t
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; --------------------------------------------------------------
;;                            Search
;; --------------------------------------------------------------

(use-package which-key
  :ensure t
  :diminish (which-key-mode)
  :config (which-key-mode))

;; Installing Counsel will bring in Ivy and Swiper as dependencies.
;; counsel, swiper and ivy come from the same repo.
(use-package counsel
  :ensure t
  :diminish (counsel-mode)
  :config
  (counsel-mode t)
  ;; Sort M-x commands by history
  (use-package amx
    :ensure t)
  )

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper)
  )

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char))

;; --------------------------------------------------------------
;;                           Completion
;; --------------------------------------------------------------

;; company is a useful auto-complete tool
(use-package company
  :ensure t
  ;; `TODO' after setting init below, company is disabled under org mode, but
  ;; quickhelp is still working
  ;; :init (setq company-global-modes '(not org-mode))
  :diminish
  ;; `TODO' it seems like the hook is useless and company mode will always start
  ;; :hook (after-init . global-company-mode)
  ;; :hook (org-mode . (lambda () (company-mode -1)))
  :hook (prog-mode . company-mode)
  :bind (("M-/" . company-complete)
	 ("<backtab>" . company-yasnippet)
	 :map company-active-map
	 ("C-p" . company-select-previous)
	 ("C-n" . company-select-next)
	 ("<tab>" . company-complete-common-or-cycle)
	 ("<backtab>" . my-company-yasnippet)
	 ;; ("C-c C-y" . my-company-yasnippet)
	 :map company-search-map
	 ("C-p" . company-select-previous)
	 ("C-n" . company-select-next))
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
	company-tooltip-limit 12            ; bigger popup window
	company-idle-delay .2               ; decrease delay before autocompletion popup shows
	company-echo-delay 0                ; remove annoying blinking
	company-minimum-prefix-length 2
	company-require-match nil
	company-dabbrev-ignore-case nil
	company-dabbrev-downcase nil))

;; show icons for company
(when (version<= "26" emacs-version)
  (use-package company-box
    :ensure t
    :diminish (company-box-mode)
    :hook (company-mode . company-box-mode)))

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
