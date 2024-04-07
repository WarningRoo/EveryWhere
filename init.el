;;; init.el

;;; Environment
;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; network
(setq url-proxy-services
      '(("https" . "localhost:7890")
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
; refresh file/dir automatically
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

;; Line-number
(column-number-mode 1)
(global-display-line-numbers-mode 1)
; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'before-save-hook (lambda () (whitespace-cleanup)))

;;; BASIC Keybindings
(global-set-key (kbd "C-c '") 'comment-or-uncomment-region)
(global-set-key (kbd "C-j") nil)
(global-set-key (kbd "C-j C-k") 'kill-whole-line)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 10)))
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 10)))

(global-set-key (kbd "M-o") 'other-window)

;; Face
(set-fringe-mode 5)
(setq default-frame-alist '((width . 90) (height . 50)))
(setq-default line-spacing 0.15)
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

(use-package dashboard
  :custom
  (dashboard-center-content t)
  (dashboard-startup-banner 'logo)
  (dashboard-set-footer nil)
  (dashboard-items '((recents   . 5)
		     (projects  . 5)
		     (bookmarks . 5)
		     (agenda    . 5)))
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
  :init (highlight-symbol-mode)
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

(use-package all-the-icons
  :config
  (when (display-graphic-p) (require 'all-the-icons)))

;; theme
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-atom")
  :config
  (load-theme 'doom-molokai t nil))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;;;LSP
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 (c-mode . lsp)
	 ;; if you want which-key integration
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

;; if you are helm user
;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)
;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;;; Project management
(use-package projectile
  :init
  (projectile-mode +1)
  (setq projectile-switch-project-action #'projectile-dired)
  :bind ((:map projectile-mode-map
	       ("C-c p" . projectile-command-map))
	 ("C-c p N" . projectile-discover-projects-in-directory)
	 )
  :config
  (setq projectile-project-search-path '(("~/Repo" . 1) ("~/src" . 1)))
					; set default dir where projects to search
  (setq projectile-auto-discover nil)   ; Suppress the auto-discovery when startup
  )

;(use-package counsel-projectile
;  :after (projectile)
;  :init
;  (counsel-projectile-mode)
;  (setq counsel-projectile-switch-project-action #'projectile-dired))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
	  treemacs-deferred-git-apply-delay        0.5
	  treemacs-directory-name-transformer      #'identity
	  treemacs-display-in-side-window          t
	  treemacs-eldoc-display                   'simple
	  treemacs-file-event-delay                2000
	  treemacs-file-extension-regex            treemacs-last-period-regex-value
	  treemacs-file-follow-delay               0.2
	  treemacs-file-name-transformer           #'identity
	  treemacs-follow-after-init               t
	  treemacs-expand-after-init               t
	  treemacs-find-workspace-method           'find-for-file-or-pick-first
	  treemacs-git-command-pipe                ""
	  treemacs-goto-tag-strategy               'refetch-index
	  treemacs-header-scroll-indicators        '(nil . "^^^^^^")
	  treemacs-hide-dot-git-directory          t
	  treemacs-indentation                     2
	  treemacs-indentation-string              " "
	  treemacs-is-never-other-window           nil
	  treemacs-max-git-entries                 5000
	  treemacs-missing-project-action          'ask
	  treemacs-move-forward-on-expand          nil
	  treemacs-no-png-images                   nil
	  treemacs-no-delete-other-windows         t
	  treemacs-project-follow-cleanup          nil
	  treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	  treemacs-position                        'left
	  treemacs-read-string-input               'from-child-frame
	  treemacs-recenter-distance               0.1
	  treemacs-recenter-after-file-follow      nil
	  treemacs-recenter-after-tag-follow       nil
	  treemacs-recenter-after-project-jump     'always
	  treemacs-recenter-after-project-expand   'on-distance
	  treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
	  treemacs-project-follow-into-home        nil
	  treemacs-show-cursor                     nil
	  treemacs-show-hidden-files               t
	  treemacs-silent-filewatch                nil
	  treemacs-silent-refresh                  nil
	  treemacs-sorting                         'alphabetic-asc
	  treemacs-select-when-already-in-treemacs 'move-back
	  treemacs-space-between-root-nodes        t
	  treemacs-tag-follow-cleanup              t
	  treemacs-tag-follow-delay                1.5
	  treemacs-text-scale                      nil
	  treemacs-user-mode-line-format           nil
	  treemacs-user-header-line-format         nil
	  treemacs-wide-toggle-width               70
	  treemacs-width                           35
	  treemacs-width-increment                 1
	  treemacs-width-is-initially-locked       t
	  treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
		 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
	("M-0"       . treemacs-select-window)
	("C-x t 1"   . treemacs-delete-other-windows)
	("C-x t t"   . treemacs)
	("C-x t d"   . treemacs-select-directory)
	("C-x t B"   . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag)))

;(use-package treemacs-evil
;  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

;(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

;;; org-mode
(defvar *dir-of-org* "~/Documents/org/")
(setq org-directory (file-truename *dir-of-org*))


(defun qu/org-font-setup()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-]\\) "
			     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (with-eval-after-load 'org-faces
    (set-face-attribute 'org-level-1 nil :height 1.6)
    (set-face-attribute 'org-level-2 nil :height 1.5)
    (set-face-attribute 'org-level-3 nil :height 1.4)
    (set-face-attribute 'org-level-4 nil :height 1.3)
    (set-face-attribute 'org-level-5 nil :height 1.2)
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
;		(org-indent-mode)
;               (variable-pitch-mode 1) ; variable pitch against fixed-pitch
		(auto-fill-mode 0)
		(visual-line-mode 1)
		(setq evil-auto-indent nil)))
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :custom
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-hide-emphasis-markers t)
  (org-ellipsis " ▾")
  (org-habit-graph-column 60)
  :config
  (require 'org-indent)
  (require 'org-habit)
  (require 'org-capture)
  ; org-tag
  (setq org-tags-column 0)
;  (setq org-tag-alist '((:startgroup . nil)
;			; Put mutually exclusive tags here
;			(:endgroup . nil)
;			("RNOW" . ?r)
;			("IDEA" . ?i)))
  ; org-todo
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'time)  ; Insert timestamp automatically when done
; (setq org-log-done 'note)  ; Insert a note with timestamp automatically when done
			     ; Use C-u C-c C-t to make it as you will.
  (setq org-log-into-drawer t)
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  ; org-agenda
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-files (list (concat org-directory "agenda")))
  (setq org-agenda-custom-commands
	'(("r" "Done within 30min." tags "+RNOW")))
  (setq org-capture-templates
	'(("t" "Todo" entry (file "~/Documents/org/agenda/tasks.org") "* TODO %?\n  %T\n")))
  (qu/org-font-setup))

(use-package org-contrib)

(use-package org-superstar
  :hook (org-mode . (lambda () (org-superstar-mode 1))))

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
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t))

(use-package visual-fill-column
  :hook
  (org-mode . (lambda () (setq visual-fill-column-width 100
			       visual-fill-column-center-text t)
		(visual-fill-column-mode 1))))

(provide 'init)
