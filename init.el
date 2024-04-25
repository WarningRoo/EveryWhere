;;; init.el -- init.el

;;; Environment
;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; network
(setq url-proxy-services '(("https" . "localhost:7890")
			   ("http" . "localhost:7890")))

;;; BASIC
(defconst *spell-check-support-enabled* nil)
(setq confirm-kill-emacs #'yes-or-no-p)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq eww-search-prefix "https://cn.bing.com/search?q=")
(electric-pair-mode t)
(save-place-mode t)
(recentf-mode t)
(delete-selection-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(savehist-mode 1)
(setq history-length 25)
(setq tab-width 8)
(setq c-basic-offset 8)
(set-input-method 'TeX)
(display-time)
(setq use-dialog-box nil)
(setq explicit-shell-file-name "/bin/bash")
;; refresh file/dir automatically
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

;; Line-number
(column-number-mode 1)
(global-display-line-numbers-mode 1)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'before-save-hook (lambda () (whitespace-cleanup)))

;;; Keybindings
(global-set-key (kbd "C-c '") 'comment-or-uncomment-region)
(global-set-key (kbd "C-j") nil)
(global-set-key (kbd "C-j C-k") 'kill-whole-line)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;;(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 10)))
;;(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 10)))

(global-set-key (kbd "M-o") 'other-window)

;; move window
;; S-M-<right/left/up/down>
(windmove-swap-states-default-keybindings '(shift meta))

;; Face
(set-fringe-mode 5)
(setq default-frame-alist '((width . 90) (height . 50)))
(set-face-attribute 'default nil
		    :font (font-spec :family "FiraCode Nerd Font Mono"
				     :foundry "Light"
				     :size 14))

;;; ABOUT Package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Package setup
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;;(use-package benchmark-init
;;  :config
;;  ;; To disable collection of benchmark data after init is done.
;;  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package dashboard
  :custom
  (dashboard-center-content t)
  (dashboard-startup-banner 'logo)

  (dashboard-icon-type 'all-the-icons)
  (dashboard-set-heading-icons t)

  (dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-todo)
  (dashboard-match-agenda-entry "+TODO=\"NOW\"")
  (dashboard-item-names '(("Agenda for the coming week:" . "Just do it.")))
  (dashboard-agenda-sort-strategy '(priority-down))
  (dashboard-agenda-prefix-format " ")

  (dashboard-items '((recents   . 5)
		     (projects  . 5)
;;		     (bookmarks . 5)
		     (agenda    . 10)))
  :config
  (dashboard-setup-startup-hook))

(use-package amx
  :init (amx-mode))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

(use-package which-key
  :init (which-key-mode))

(use-package highlight-symbol
  :bind ("<f9>" . highlight-symbol))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package tree-sitter-indent)

(use-package tree-sitter)

(use-package magit)

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

(use-package good-scroll
  :bind (([next] . #'good-scroll-up-full-screen)
	 ([prior] . #'good-scroll-down-full-screen))
  :config
  (good-scroll-mode 1))

(use-package slime
  :defer t
  :config
  (setq inferior-lisp-program (executable-find "sbcl")))

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
  :init (ivy-rich-mode t)
  :config (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package company
  :init (global-company-mode)
  :hook ((after-init-hook . global-company-mode)))

(use-package company-box
  :hook ((company-mode-hook . company-box-mode)))

;; theme
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-tokyo-night t nil))

(use-package rich-minority
  :init
  (rich-minority-mode 1)
  :config
  (setq rm-blacklist
	(format "^ \\(%s\\)$"
		(mapconcat #'identity
			   '("ivy" "WK" "counsel" "company" "Abbrev" "Eldoc" "org-roam-ui")
			   "\\|"))))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode))

;;; org-mode
(defvar *dir-of-org* "~/Documents/org/")
(setq org-directory (file-truename *dir-of-org*))

(defun qu/org-font-setup()
  "Replace list hyphen with dot."
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-]\\) "
			     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

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
  :bind (;("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :custom
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-hide-emphasis-markers t)
  (org-ellipsis " ▾")
  (org-log-done 'time)	;Insert timestamp automatically when done
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
  ;;  (setq org-tag-alist '((:startgroup . nil)
  ;;			; Put mutually exclusive tags here
  ;;			(:endgroup . nil)
  ;;			("RNOW" . ?r)
  ;;			("IDEA" . ?i)))
  (setq org-todo-keywords '((sequence "TODO(t)" "NOW(n)" "|" "DONE(d)" "CANCELED(c)")))
  (setq org-agenda-custom-commands
	'(("n" "Now you are doing." todo "NOW")))
  (setq org-capture-templates
	'(("t" "Todo" entry (file "~/Documents/org/agenda/tasks.org") "* TODO %?\n  %T\n" :prepend t)
	  ("i" "Idea" entry (file "~/Documents/org/agenda/ideas.org") "* %T\n" :prepend t)))
  (qu/org-font-setup)

  ;; require Programming Languages Support
  (require 'ob-lisp)
  (require 'ob-shell))

(use-package org-contrib)

(use-package org-superstar
  :hook (org-mode . (lambda () (org-superstar-mode 1))))

(use-package org-pomodoro)

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
  (org-mode . (lambda () (setq visual-fill-column-width 100
			       visual-fill-column-center-text t)
		(visual-fill-column-mode 1))))

(use-package adaptive-wrap)

(provide 'init)
;;; init.el ends here
