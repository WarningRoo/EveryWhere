(eval-when-compile 
 (require 'use-package))

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
;(add-to-list 'default-frame-alist '(width . 90))
;(add-to-list 'default-frame-alist '(height . 55))
(setq tab-width 8)
(setq c-basic-offset 4)

;; BASIC Keybindings
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

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(when (display-graphic-p) (load-theme 'dracula t))
(when (display-graphic-p) (require 'all-the-icons))
(add-hook 'after-init-hook 'global-company-mode)
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

;; IVY
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
(setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(good-scroll-mode 1)
(global-set-key [next] #'good-scroll-up-full-screen)
(global-set-key [prior] #'good-scroll-down-full-screen)

(setq chatgpt-shell-openai-key "i have no key here")

(google-this-mode 1)

(require 'google-translate)
(require 'google-translate-default-ui)
(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "zh-CN")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(tree-sitter-indent tree-sitter org-roam use-package google-translate google-this magit company-box good-scroll counsel swiper ivy company all-the-icons dracula-theme cmake-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; org-mode
;(require 'init-org)
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
;;; init.el ends here
