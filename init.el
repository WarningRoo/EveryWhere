(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; BASIC configuration
(defconst *spell-check-support-enabled* nil)
(setq confirm-kill-emacs #'yes-or-no-p)
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(column-number-mode t)
(electric-pair-mode t)
(add-hook 'prog-mode-hook #'show-paren-mode)
(global-auto-revert-mode t) ;当另一程序修改了文件时，让 Emacs 及时刷新 Buffer
(delete-selection-mode t)
(global-display-line-numbers-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(savehist-mode 1)
(setq tab-width 8)
(setq c-basic-offset 4)

;;; BASIC Keybindings
(global-set-key (kbd "C-c '") 'comment-or-uncomment-region)
(global-set-key (kbd "C-j") nil)
(global-set-key (kbd "C-j C-k") 'kill-whole-line)

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
 '(package-selected-packages
   '(dashboard amx molokai-theme tree-sitter-indent tree-sitter org-roam use-package google-translate google-this magit company-box good-scroll counsel swiper ivy company all-the-icons dracula-theme cmake-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Package setup
(eval-when-compile 
  (require 'use-package))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package amx
  :ensure t
  :init (amx-mode))

(use-package tree-sitter-indent
  :ensure t)
(use-package tree-sitter
  :ensure t)
(use-package magit
  :ensure t)
(use-package pdf-tools
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
  :hook (('after-init-hook . 'global-company-mode)))

(use-package company-box
  :ensure t
  :hook (('company-mode-hook . 'company-box-mode)))

(use-package all-the-icons
  :ensure t
  :config
  (when (display-graphic-p) (require 'all-the-icons)))

(use-package dracula-theme
  :ensure t)

(use-package molokai-theme
  :ensure t)

;; theme set
(when (display-graphic-p) (load-theme 'dracula t))
;(when (display-graphic-p) (load-theme 'molokai t))

;;; org-mode
(defvar *dir-of-org* "~/Documents/org/")
(setq org-directory (file-truename *dir-of-org*))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename *dir-of-org*))
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

(provide 'init)
