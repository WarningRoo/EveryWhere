;;; init.el -- init.el

;;; Commentary:
;; Less is more

;;; Code:

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Face
(set-fringe-mode 8)
(setq default-frame-alist '((width . 100) (height . 46)))

;; Fonts
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun qu/font-setup ()
  "Setup fonts."
  ;; Set default font
  (cl-loop for font in '("Jetbrains Mono" "Consolas")
	   when (font-installed-p font)
	   return (set-face-attribute 'default nil
				      :font (font-spec :family font
						       :weight 'Regular
						       :size 15)))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Jetbrains Mono" "Segoe UI Symbol" "Symbola" "Symbol")
	   when (font-installed-p font)
	   return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))

  ;; Emoji
  (cl-loop for font in '("Noto Color Emoji" "Segoe UI Emoji")
	   when (font-installed-p font)
	   return (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("Sarasa Term SC Nerd" "Microsoft Yahei UI" "Simhei")
	   when (font-installed-p font)
	   return (progn (set-fontset-font t 'cjk-misc (font-spec :family font))
			 (set-fontset-font t 'han (font-spec :family font)))))

(qu/font-setup)
(add-hook 'window-setup-hook #'qu/font-setup)
(add-hook 'server-after-make-frame-hook #'qu/font-setup)

;;; BASIC
(setq inhibit-startup-message t)
(setq confirm-kill-emacs #'yes-or-no-p)
(setq make-backup-files nil)
(setq eww-search-prefix "https://cn.bing.com/search?q=")
(setq package-check-signature nil)
(electric-pair-mode -1)
(save-place-mode t)
(recentf-mode t)
(delete-selection-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq history-length 25)
(setq tab-width 4)
(set-input-method 'TeX)
(display-time)
(setq use-dialog-box nil)
(global-hl-line-mode 0)
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

;;; line/column number
(column-number-mode 1)
;; Enable line numbers for some modes
(dolist (mode '(prog-mode-hook
		diff-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'before-save-hook (lambda () (whitespace-cleanup)))

;;; Keybindings
(global-set-key (kbd "C-c '") 'comment-or-uncomment-region)
(global-set-key (kbd "M-o") 'other-window)

;; Pixel scroll
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)
(defalias 'scroll-up-command 'pixel-scroll-interpolate-down)
(defalias 'scroll-down-command 'pixel-scroll-interpolate-up)

;; move window
;; S-M-<right/left/up/down>
(windmove-swap-states-default-keybindings '(shift meta))

;; ABOUT Package
(eval-when-compile (require 'use-package))
(use-package package
  :config
  (setq use-package-always-ensure t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (unless (bound-and-true-p package--initialized)
    (package-initialize)))

(use-package benchmark-init
  :disabled
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package gptel
  :config
  (setq gptel-model "moonshot-v1-8k")
  (setq gptel-default-mode 'org-mode)
  (setq gptel-backend
	(gptel-make-openai "Moonshot"
	  :key 'gptel-api-key
	  :models '("moonshot-v1-8k"
		    "moonshot-v1-32k"
		    "moonshot-v1-128k")
	  :host "api.moonshot.cn")))

(use-package popper
  :bind (("C-`"   . popper-toggle)
	 ("M-`"   . popper-cycle)
	 ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
	'("\\*Google Translate\\*"
	  "\\*Messages\\*"
	  "Output\\*$"
	  "\\*Async Shell Command\\*"
	  "\\*Buffer List\\*"
	  "\\*Backtrace\\*"
	  help-mode
	  compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package exec-path-from-shell
  :after eshell
  :config
  (exec-path-from-shell-copy-env "ARCH")
  (exec-path-from-shell-copy-env "CROSS_COMPILE")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package all-the-icons
  :if (display-graphic-p))


(use-package dashboard
  :custom
  (dashboard-center-content t)
  (dashboard-startup-banner 'logo)
  (dashboard-banner-logo-title "Practice")
  ;; Agenda
  (dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-todo)
  (dashboard-match-agenda-entry "+TODO=\"NOW\"")
  (dashboard-agenda-sort-strategy '(priority-down))
  (dashboard-agenda-prefix-format " ")
  ;; Homepage
  (dashboard-items '((recents  . 5)
		     (projects . 5)
		     (agenda   . 10)))
  (dashboard-startupify-list '(dashboard-insert-banner
			       dashboard-insert-banner-title
			       (lambda () (delete-char -1))
			       dashboard-insert-items
			       dashboard-insert-init-info))
  :config
  (dashboard-setup-startup-hook))

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
	 ("C-e" . mwim-end-of-code-or-line)))

(use-package highlight-symbol
  :bind ("<f9>" . highlight-symbol))

(use-package magit
  :defer t)

(use-package google-translate
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "zh-CN")
  (google-translate-output-destination nil)
  (google-translate-show-phonetic t)
  :config
  (require 'google-translate-smooth-ui)
  :bind (("\C-ct" . google-translate-at-point)
	 ("\C-cT" . google-translate-query-translate)))

(use-package slime
  :defer t
  :config
  (setq inferior-lisp-program (executable-find "sbcl")))

(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
	 ;;?("C-c M-x" . consult-mode-command)
	 ;;?("C-c h" . consult-history)
	 ;;?("C-c k" . consult-kmacro)
	 ("C-c m" . consult-man)
	 ("C-c i" . consult-info)
	 ([remap Info-search] . consult-info)
	 ;; C-x bindings in `ctl-x-map'
	 ;;!("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ;;!("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ;;!("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ;;!("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ;;!("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
	 ;;!("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ;;!("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ;; orig. yank-pop
	 ;; M-g bindings in `goto-map'
	 ("M-g e" . consult-compile-error) ;; Cycling between compile error(s)/warning(s)
	 ("M-g f" . consult-flymake) ;; Cycling between flymake results
	 ("M-g g" . consult-goto-line)             ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings in `search-map'
	 ("M-s d" . consult-find)                  ;; Alternative: consult-fd
	 ;;!("M-s c" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("C-s" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 ;;!:map isearch-mode-map
	 ;;!("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
	 ;;!("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
	 ;;!("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
	 ;;!("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 ;;!:map minibuffer-local-map
	 ;;!("M-s" . consult-history)                 ;; orig. next-matching-history-element
	 ;;!("M-r" . consult-history)                ;; orig. previous-matching-history-element
	 )
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init

  ;;; Registers Related
  (setq register-preview-delay 0.5)
  ;; Set the separator for register append/prepend
  (setq register-separator ?+)
  (set-register register-separator "\n")

  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package embark
  :bind  (("C-." . embark-act)         ;; pick some comfortable binding
	  ("C-;" . embark-dwim)        ;; good alternative: M-.
	  ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-eglot)
(use-package consult-eglot-embark
  :init
  (consult-eglot-embark-mode))

(use-package company
  :hook (after-init . global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; theme
(use-package dracula-theme
  :config
  ;;(load-theme 'wombat t)
  (load-theme 'dracula t))


(use-package rich-minority
  :init
  (rich-minority-mode 1)
  :config
  (setq rm-blacklist
	(format "^ \\(%s\\)$"
		(mapconcat #'identity
			   '("company" "Abbrev" "Eldoc" "org-roam-ui" "company-box" "hs" "Wrap")
			   "\\|"))))

(use-package project
  :config
  (setq project-vc-extra-root-markers '("INSTALL" "COPYING" "LICENSE")))

(use-package neotree
  :after project
  :bind ("<f8>" . neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  (setq neo-autorefresh t))

;; eglot lsp related
(use-package eglot
  :hook
  ((c-mode c-ts-mode) . eglot-ensure)
  ((c++-mode c++-ts-mode) . eglot-ensure)
  ((lisp-mode emacs-lisp-mode) . eglot-ensure)
  :config
  (setq eglot-autoshutdown t)
  ;; Add server here
  (add-to-list
   'eglot-server-programs
   ;;   '((c++-mode c++-ts-mode c-mode c-ts-mode) "clangd"
   ;;     "--limit-references=1000"
   ;;     "--limit-results=1000"
   ;;     "--background-index"
   ;;     )
   '((c++-mode c++-ts-mode c-mode c-ts-mode) "ccls")
   ;; '((lisp-mode emacs-lisp-mode) "sbcl"
   ;;   "--noinform"
   ;;   "--eval" "ql:quickload \"alive-lsp\""
   ;;   "--eval" "(alive/server::start :port 8006)")
   ))

(use-package flymake
  :custom
  (flymake-mode-line-lighter "F")
  :hook (prog-mode . flymake-mode)
  :bind (("M-n" . flymake-goto-next-error)
	 ("M-p" . flymake-goto-prev-error)))

(use-package treesit-auto
  :if (eq system-type 'gnu/linux)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; LaTeX
(use-package latex
  :defer t
  :ensure auctex)

(use-package cdlatex)

;;; org-mode
(defvar *dir-of-org* "~/Documents/Knowing/")
(setq org-directory (file-truename *dir-of-org*))

(defun qu/org-font-setup()
  "Font set for org."
  ;; emphasis
  (add-to-list 'org-emphasis-alist '("*" '(bold :foreground "#00BFFF")))
  (add-to-list 'org-emphasis-alist '("/" '(italic :foreground "#00FF22")))
  ;; Title
  (with-eval-after-load 'org-faces
    (set-face-attribute 'org-level-1 nil :height 1.4)
    (set-face-attribute 'org-level-2 nil :height 1.3)
    (set-face-attribute 'org-level-3 nil :height 1.2)
    (set-face-attribute 'org-level-4 nil :height 1.1)
    (set-face-attribute 'org-level-5 nil :height 1.1)
    (set-face-attribute 'org-level-6 nil :height 1.1)
    (set-face-attribute 'org-level-7 nil :height 1.1)
    (set-face-attribute 'org-level-8 nil :height 1.1))
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-special-keyword nil
		      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil
		      :inherit '(font-lock-comment-face fixed-pitch)))

(use-package org
  :hook
  (org-mode . (lambda ()
		;;(org-indent-mode)
		;;(variable-pitch-mode 1) ; variable pitch against fixed-pitch
		(auto-fill-mode 0)
		(visual-line-mode 1)
		(setq-local line-spacing 0.10)))
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :custom
  (org-imenu-depth 3)
  (org-default-notes-file (concat org-directory "agenda/ideas.org"))
  (org-hide-emphasis-markers t)
  ;;(org-log-done 'time)
  ;;(org-log-done 'note)
  ;;Use C-u C-c C-t to make it as you will.
  (org-tags-column 0)
  (org-enforce-todo-dependencies t)
  (org-log-into-drawer t)
  (org-agenda-start-with-log-mode t)
  (org-agenda-files (list (concat org-directory "agenda/")))

  :config
  ;; LaTeX
  (setq org-highlight-latex-and-related '(native))
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-latex-packages-alist '(("T1" "fontenc" t)
				   ("" "amsmath" t)
				   ("" "mathtools" t)
				   ("" "siunitx" t)
				   ("" "newtxmath" t)
				   ("" "tikz" t)))
  (plist-put org-format-latex-options :scale 1.5)

  ;; require module
  (require 'org-indent)
  (require 'org-capture)

  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  ;; (setq org-tag-alist '((:startgroup . nil)
  ;;			;; Put mutually exclusive tags here
  ;;			(:endgroup . nil)
  ;;			("RNOW" . ?r)
  ;;			("IDEA" . ?i)))
  (setq org-todo-keywords '((sequence "TODO(t)" "NOW(n)" "|" "DONE(d)" "CANCELED(c)" "FUTURE(f)")))
  (setq org-agenda-custom-commands
	'(("n" "Now you are doing." todo "NOW")
	  ("f" "Maybe someday" todo "FUTURE")))

  (setq org-capture-templates
	`(("t" "Todo" entry (file ,(concat org-directory "agenda/tasks.org")) "* TODO %?\n  %T\n" :prepend t)
	  ("i" "Idea" entry (file ,(concat org-directory "agenda/ideas.org")) "* %T\n" :prepend t)))

  (qu/org-font-setup)

  ;; require Programming Languages Support
  (require 'ob-lisp)
  (require 'ob-shell)
  (require 'ob-makefile))

(use-package org-contrib)

(use-package org-roam
  :custom
  (org-roam-directory (concat org-directory "roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n c" . org-roam-capture)
	 ;; Dailies
	 ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template
	(concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package visual-fill-column
  :disabled
  :hook
  (org-mode . (lambda () (setq visual-fill-column-width 110
			       visual-fill-column-center-text t)
		(visual-fill-column-mode 1))))

(use-package adaptive-wrap
  :hook (org-mode . (lambda () (adaptive-wrap-prefix-mode 1))))

(provide 'init)
;;; init.el ends here
