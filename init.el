(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; BASIC configuration
(defconst *spell-check-support-enabled* nil)
(setq confirm-kill-emacs #'yes-or-no-p)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq eww-search-prefix "https://cn.bing.com/search?q=")
(column-number-mode t)
(electric-pair-mode t)
(add-hook 'prog-mode-hook #'show-paren-mode)
(global-auto-revert-mode t) ;当另一程序修改了文件时，让 Emacs 及时刷新 Buffer
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(delete-selection-mode t)
(global-display-line-numbers-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(savehist-mode 1)
(setq tab-width 8)
(setq c-basic-offset 4)
(set-input-method 'TeX)
(display-time)

;;; BASIC Keybindings
(global-set-key (kbd "C-c '") 'comment-or-uncomment-region)
(global-set-key (kbd "C-j") nil)
(global-set-key (kbd "C-j C-k") 'kill-whole-line)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

(defun next-ten-lines()
  "Move cursor to next 10 lines."
  (interactive)
  (next-line 10))

(defun previous-ten-lines()
  "Move cursor to previous 10 lines."
  (interactive)
  (previous-line 10))

(global-set-key (kbd "M-n") 'next-ten-lines)
(global-set-key (kbd "M-p") 'previous-ten-lines)

;;; ABOUT Package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-display-line-numbers-mode t)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style 'relative)
 '(package-selected-packages
   '(org-roam-ui align which-key tablist rainbow-delimiters org org-contrib slime treemacs-tab-bar treemacs-magit treemacs-icons-dired treemacs-projectile treemacs lsp-ivy lsp-ui counsel-projectile projectile lsp-mode highlight-symbol mwim dashboard amx molokai-theme tree-sitter-indent tree-sitter org-roam use-package google-translate google-this magit company-box good-scroll counsel swiper ivy company all-the-icons dracula-theme cmake-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "FiraCode Nerd Font Mono" :foundry "CTDB" :slant normal :weight regular :height 110 :width normal))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :height 1.6))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2)))))

;;; Package setup
(eval-when-compile
  (require 'use-package))

(use-package dashboard
; :disabled
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-startup-banner nil)
  (setq dashboard-center-content t)
  (setq dashboard-set-footer nil))

(use-package amx
  :ensure t
  :init (amx-mode))

(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package highlight-symbol
  :ensure t
  :init (highlight-symbol-mode)
  :bind ("<f9>" . highlight-symbol))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package tree-sitter-indent
  :ensure t)

(use-package tree-sitter
  :ensure t)

(use-package magit
  :ensure t)

(use-package google-translate
  :ensure t
  :init
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "zh-CN")
  (setq google-translate-output-destination 'popup)
  :bind (("\C-ct" . 'google-translate-at-point)
         ("\C-cT" . 'google-translate-query-translate))
  :config
  (require 'google-translate-default-ui))

(use-package google-this
  :ensure t
  :config
  (google-this-mode 1))

(use-package good-scroll
  :ensure t
  :bind (([next] . #'good-scroll-up-full-screen)
         ([prior] . #'good-scroll-down-full-screen))
  :config
  (good-scroll-mode 1))

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  (setq search-default-mode #'char-fold-to-regexp)
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

(use-package company
  :ensure t
  :init (global-company-mode))
;  :hook (after-init-hook . global-company-mode))

;(add-hook 'after-init-hook 'global-company-mode)

(use-package company-box
  :ensure t
  :hook ((company-mode-hook . company-box-mode)))

(use-package all-the-icons
  :ensure t
  :config
  (when (display-graphic-p) (require 'all-the-icons)))

(use-package dracula-theme
  :ensure t)

(use-package molokai-theme
  :ensure t)

;; theme set
;(when (display-graphic-p) (load-theme 'dracula t))
(when (display-graphic-p) (load-theme 'molokai t))
(load-theme 'molokai t)

;;;LSP
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
;; if you are helm user
;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)
;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :init (counsel-projectile-mode))

(use-package treemacs
  :ensure t
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
;  :after (treemacs evil)
;  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;  :ensure t
;  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package slime
  :ensure t
  :config (setq inferior-lisp-program (executable-find "sbcl")))

; org-mode
(defvar *dir-of-org* "~/Documents/org/")
(setq org-directory (file-truename *dir-of-org*))
(add-hook 'org-mode-hook #'auto-fill-mode)

(use-package org
  :ensure t)

(use-package org-contrib
  :ensure t)

(use-package org-roam
  :ensure t
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
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(provide 'init)
