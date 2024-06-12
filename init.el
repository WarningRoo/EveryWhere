;;; init.el -- init.el

;;; Commentary:
;; Less is more

;;; Code:

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Proxy
(setq url-proxy-services '(("https" . "localhost:7890")
			   ("http" . "localhost:7890")))

;; Face
(set-fringe-mode 4)
(setq default-frame-alist '((width . 114) (height . 55)))

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
						       :size 14)))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Jetbrains Mono" "Segoe UI Symbol" "Symbola" "Symbol")
	   when (font-installed-p font)
	   return (set-fontset-font t 'symbol (font-spec :family font :size 14) nil 'prepend))

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
(electric-pair-mode -1)
(save-place-mode t)
(recentf-mode t)
(delete-selection-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(savehist-mode 1)
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
(dolist (mode '(prog-mode-hook))
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
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (unless (bound-and-true-p package--initialized)
    (package-initialize)))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package benchmark-init
  :disabled
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package dashboard
;;  :disabled
  :custom
  (dashboard-center-content t)
  (dashboard-icon-type 'all-the-icons)
  (dashboard-banner-logo-title "HE JUST DID IT.\n一具体，就深刻。")
  (dashboard-set-heading-icons t)
  (dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-todo)
  (dashboard-match-agenda-entry "+TODO=\"NOW\"")
  (dashboard-agenda-sort-strategy '(priority-down))
  (dashboard-agenda-prefix-format " ")
  (dashboard-items '((recents  . 10)
		     ;;(projects . 10)
		     (agenda   . 30)))
  (dashboard-startupify-list '(dashboard-insert-newline
			       dashboard-insert-banner-title
			       dashboard-insert-items
			       dashboard-insert-newline
			       dashboard-insert-init-info))
  :config
  (dashboard-setup-startup-hook))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

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
  :bind (("\C-ct" . 'google-translate-at-point)
	 ("\C-cT" . 'google-translate-query-translate)))

(use-package slime
  :defer t
  :config
  (setq inferior-lisp-program (executable-find "sbcl")))

(use-package amx
  :init (amx-mode))

(use-package swiper)
(use-package counsel)
(use-package ivy
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-use-selectable-prompt t)
  ;; enable this if you want `swiper' to use it
  (search-default-mode #'char-fold-to-regexp)
  :config
  :bind (("C-s" . 'swiper)
	 ("C-c C-r" . 'ivy-resume)
	 ("<f6>" . 'ivy-resume)
	 ("M-x" . 'counsel-M-x)
	 ("C-x C-f" . 'counsel-find-file)
	 ("<f1> f" . 'counsel-describe-function)
	 ("<f1> v" . 'counsel-describe-variable)
	 ("<f1> o" . 'counsel-describe-symbol)
	 ("<f1> l" . 'counsel-find-library)
	 ("<f2> i" . 'counsel-info-lookup-symbol)
	 ("<f2> u" . 'counsel-unicode-char)
	 ("C-c g" . 'counsel-git)
	 ("C-c j" . 'counsel-git-grep)
	 ("C-c k" . 'counsel-ag)
	 ("C-x l" . 'counsel-locate)
	 ("C-S-o" . 'counsel-rhythmbox)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package company
  :hook (after-init . global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; theme
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-tokyo-night t))

(use-package rich-minority
  :init
  (rich-minority-mode 1)
  :config
  (setq rm-blacklist
	(format "^ \\(%s\\)$"
		(mapconcat #'identity
			   '("ivy" "WK" "counsel" "company" "Abbrev" "Eldoc" "org-roam-ui" "company-box" "hs" "Wrap")
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
  ;; Add sever here
  (add-to-list
   'eglot-server-programs
   '((c++-mode c++-ts-mode c-mode c-ts-mode) "clangd")
   ;;'((c++-mode c++-ts-mode c-mode c-ts-mode) "ccls")
   ;; '((lisp-mode emacs-lisp-mode) "sbcl"
   ;;   "--noinform"
   ;;   "--eval" "ql:quickload \"alive-lsp\""
   ;;   "--eval" "(alive/server::start :port 8006)")
   ))

(use-package flymake
  :custom
  (flymake-mode-line-lighter "F")
  :hook
  (prog-mode . flymake-mode)
  :bind
  ("M-n" . flymake-goto-next-error)
  ("M-p" . flymake-goto-prev-error))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; org-mode
(defvar *dir-of-org* "~/Documents/Knowing/")
(setq org-directory (file-truename *dir-of-org*))

(defun qu/org-font-setup()
  "Font set for org."
  ;; LaTeX preview
  (plist-put org-format-latex-options :scale 1.5)
  (setq org-preview-latex-default-process 'dvisvgm)
  ;; emphasis
  (add-to-list 'org-emphasis-alist '("*" '(bold :foreground "#00BFFF")))
  (add-to-list 'org-emphasis-alist '("/" '(italic :foreground "#e50062")))
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
		(adaptive-wrap-prefix-mode)
		(setq evil-auto-indent nil)
		(setq-local line-spacing 0.10)))
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :custom
  (org-imenu-depth 3)
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-hide-emphasis-markers t)
  ;;(org-ellipsis " ▾")
  ;;(org-log-done 'time)
  ;;(org-log-done 'note)
  ;;Use C-u C-c C-t to make it as you will.
  (org-tags-column 0)
  (org-enforce-todo-dependencies t)
  (org-log-into-drawer t)
  (org-agenda-start-with-log-mode t)
  (org-agenda-files (list (concat org-directory "agenda")))

  :config
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
	'(("t" "Todo" entry (file "~/Documents/Knowing/agenda/tasks.org") "* TODO %?\n  %T\n" :prepend t)
	  ("i" "Idea" entry (file "~/Documents/Knowing/agenda/ideas.org") "* %T\n" :prepend t)))
  (qu/org-font-setup)

  ;; require Programming Languages Support
  (require 'ob-lisp)
  (require 'ob-shell)
  (require 'ob-makefile))

(use-package org-contrib)

(use-package org-superstar
  :hook (org-mode . (lambda () (org-superstar-mode 1))))

(use-package org-roam
  :defer t
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
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

(use-package visual-fill-column
  :hook
  (org-mode . (lambda () (setq visual-fill-column-width 110
			       visual-fill-column-center-text t)
		(visual-fill-column-mode 1))))

(use-package adaptive-wrap)

(provide 'init)
;;; init.el ends here
