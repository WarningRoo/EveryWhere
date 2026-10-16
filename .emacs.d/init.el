;;; init.el --- A Practical and Fast Emacs Configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Fonts
(defun qu/first-available (&rest fonts)
  "Return the first font in FONTS that exists in the current fontset."
  (seq-find (lambda (font) (find-font (font-spec :name font))) fonts))

(defun qu/font-setup (&optional frame)
  "Configure fonts on FRAME by selecting the first available font."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (when-let*
          ((font (qu/first-available "Maple Mono NF CN" "Noto Sans Mono" "Jetbrains Mono" "Consolas")))
        (set-frame-font (font-spec :family font :size 15.0 :weight 'regular) nil)
        (set-face-attribute 'fixed-pitch (selected-frame) :family font))
      (when-let*
          ((font (qu/first-available "Noto Color Emoji" "Segoe UI Emoji")))
        (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))
      (when-let*
          ((font (qu/first-available "Maple Mono NF CN" "Noto Sans CJK SC" "Sarasa Term SC Nerd" "Microsoft Yahei UI")))
        (set-fontset-font t 'han (font-spec :family font) nil 'prepend)
        (set-fontset-font t 'cjk-misc (font-spec :family font) nil 'prepend))
      (when-let*
          ((font (qu/first-available "Inter" "Noto Sans" "Segoe UI")))
        (dolist (face '(mode-line mode-line-inactive tab-line header-line))
          (set-face-attribute face (selected-frame)
                              :family font :height 130 :weight 'regular))))))

(add-hook 'after-make-frame-functions #'qu/font-setup)
(when (display-graphic-p) (qu/font-setup))

;;; BASIC
(setq confirm-kill-emacs #'yes-or-no-p)
(setq make-backup-files nil)
(setq history-length 25)
(setq use-dialog-box nil)
(setq-default indent-tabs-mode nil)
(setq-default cursor-type 'bar)
(save-place-mode t)
(setq recentf-auto-cleanup 'never)
(recentf-mode t)
(delete-selection-mode t)
(show-paren-mode)
(setq read-extended-command-predicate #'command-completion-default-include-p)

(defun qu/enable-tex-input-method ()
  "Enable TeX input method in the current buffer."
  (set-input-method "TeX"))

(dolist (hook '(text-mode-hook org-mode-hook LaTeX-mode-hook latex-mode-hook))
  (add-hook hook #'qu/enable-tex-input-method))

;; auto-revert
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

;; Display the column indicator
(setq-default fill-column 120)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;;; line/column number
(setq display-line-numbers-width-start t
      display-line-numbers-grow-only t)
(column-number-mode 1)

;; Enable line numbers for some modes
(dolist (mode '(prog-mode-hook
                diff-mode-hook))
  (add-hook mode #'display-line-numbers-mode))

(setq delete-trailing-lines nil)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'delete-trailing-whitespace-mode))

(global-set-key (kbd "C-c '") 'comment-or-uncomment-region)

;; Pixel scroll
(setq scroll-margin 3
      scroll-conservatively 101)
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)
(defalias 'scroll-up-command 'pixel-scroll-interpolate-down)
(defalias 'scroll-down-command 'pixel-scroll-interpolate-up)

;; Scroll line
(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-n") 'scroll-up-line)

;; ABOUT Package
(require 'use-package)
(require 'bind-key)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-install-upgrade-built-in nil)
(setq package-check-signature nil)
(unless (bound-and-true-p package--initialized)
    (package-initialize))
(setq use-package-always-ensure nil)

(use-package benchmark-init
  :disabled
  :ensure t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package gcmh
  :ensure t
  :init (gcmh-mode 1))

(use-package gdb-mi
  :config
  (setq gdb-many-windows t)
  (setq gdb-show-main t))

(use-package tramp
  :config
  (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t))

(use-package winner
  :init (winner-mode))

(use-package mouse
  :custom
  (window-resize-pixelwise t)
  (mouse-1-double-click-prefer-symbols t)
  :config
  (context-menu-mode 1)
  (mouse-shift-adjust-mode 1))

(use-package tab-line
  :demand t
  :bind (:map tab-line-mode-map
              ("C-<prior>" . tab-line-switch-to-prev-tab)
              ("C-<next>" . tab-line-switch-to-next-tab))
  :custom
  (tab-line-close-button-show 'selected)
  (tab-line-tab-name-function #'tab-line-tab-name-truncated-buffer)
  (tab-line-tab-name-truncated-max 24)
  (tab-line-exclude-buffers
   (lambda (buffer) (not (buffer-file-name (get-buffer buffer)))))
  (tab-line-tabs-window-buffers-filter-function #'tab-line-tabs-non-excluded)
  :config
  (custom-set-faces
   '(tab-line ((t (:background "#282828" :foreground "#b0bec5" :box nil))))
   '(tab-line-tab ((t (:inherit tab-line :background "#282828" :box nil))))
   '(tab-line-tab-inactive ((t (:inherit tab-line-tab :foreground "#9e9e9e" :background "#282828"))))
   '(tab-line-tab-current ((t (:inherit tab-line-tab :foreground "#eeffff" :background "#3a3a3a"))))
   '(tab-line-highlight ((t (:background "#424242" :box nil)))))
  (global-tab-line-mode 1))

(use-package prog-mode
  :hook (prog-mode . (lambda () (setq-local line-spacing '(0.03 . 0.03)))))

(use-package xref
  :hook (prog-mode . xref-mouse-mode)
  :config
  (defun qu/context-menu-xref (menu click)
    "Add Xref actions for the symbol at mouse CLICK to MENU."
    (when (and (derived-mode-p 'prog-mode)
               (bounds-of-thing-at-mouse click 'symbol))
      (define-key-after menu [xref-separator] menu-bar-separator)
      (define-key-after menu [xref-definition]
        `(menu-item "Go to Definition"
                    ,(lambda () (interactive) (xref-find-definitions-at-mouse click))))
      (define-key-after menu [xref-references]
        `(menu-item "Find References"
                    ,(lambda () (interactive) (xref-find-references-at-mouse click)))))
    menu)
  (add-hook 'context-menu-functions #'qu/context-menu-xref))

(use-package which-func
  :custom
  (which-func-modes '(prog-mode))
  (which-func-display 'header)
  :config
  (which-function-mode 1))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?j ?k ?l))
  (aw-background t)
  (aw-scope 'frame)
  (aw-minibuffer-flag t)
  (aw-dispatch-always t)
  :custom-face
  (aw-leading-char-face
   ((t (:foreground "orange red" :weight bold :height 2.0)))))

(use-package dired
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-dwim-target t))

(use-package dirvish
  :ensure t
  :custom (dirvish-side-width 32)
  :init
  (dirvish-override-dired-mode)
  :bind (("C-c d" . dirvish-side)
         :map dirvish-mode-map
         ("<mouse-2>" . qu/dirvish-mouse-open))
  :config
  (require 'dirvish-subtree)
  (defun qu/dirvish-mouse-open (event)
    "Expand the directory at EVENT, or open its file in the editor."
    (interactive "e")
    (mouse-set-point event)
    (if (file-directory-p (dired-get-file-for-visit))
        (dirvish-subtree-toggle)
      (dired-find-file)))
  (dirvish-side-follow-mode))

(use-package eww
  :hook
  (eww-mode . visual-line-mode)
  :config
  (setq eww-search-prefix "https://cn.bing.com/search?q=")
  (setq shr-width 80)
  (setq shr-indentation 2)
  (setq shr-max-width fill-column)
  (setq shr-use-fonts nil))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.05))

(use-package hl-line
  :hook ((prog-mode dired-mode) . hl-line-mode)
  :custom (hl-line-sticky-flag nil)
  :config
  (custom-set-faces
   '(hl-line ((t (:background "#282828" :extend t))))))

(use-package aidermacs
  :ensure t
  :defer t
  :custom
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "sonnet"))

(use-package gptel
  :ensure t
  :defer t)

(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-window-height
   (lambda (win)
     (let ((height (floor (frame-height (window-frame win)) 3)))
       (fit-window-to-buffer win height height))))
  (popper-display-function
   (lambda (buffer &optional alist)
     ;; Keep callers such as Flymake from overriding the shared panel height.
     (popper-select-popup-at-bottom
      buffer (cons (cons 'window-height popper-window-height) alist))))
  :init
  (setq popper-reference-buffers
        '("\\*gt-result\\*"
          "\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Buffer List\\*"
          "\\*Backtrace\\*"
          "\\`\\*eldoc\\*"
          "\\`\\*company-documentation\\*\\'"
          help-mode
          compilation-mode
          flymake-diagnostics-buffer-mode
          flymake-project-diagnostics-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-envs
   '("ARCH" "CROSS_COMPILE"
     "ANTHROPIC_AUTH_TOKEN" "ANTHROPIC_DEFAULT_HAIKU_MODEL"))
  (when (or (memq window-system '(mac ns x pgtk))
            (daemonp))
    (exec-path-from-shell-initialize)))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(use-package go-translate
  :ensure t
  :bind (("C-c t" . gt-do-translate))
  :config
  (setq gt-default-translator (gt-translator
                               :taker   (gt-taker :langs '(en zh))
                               :engines (list (gt-youdao-dict-engine))
                               :render  (gt-buffer-render))))

(use-package slime
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program (executable-find "sbcl")))

(use-package vertico
  :ensure t
  :custom (vertico-resize nil)
  :init
  (vertico-mode))

(use-package vertico-posframe
  :disabled
  :ensure t
  :init
  (vertico-posframe-mode 1))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ;;?("C-c M-x" . consult-mode-command)
         ;;?("C-c h" . consult-history)
         ;;?("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ;;!("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error) ;; Cycling between compile error(s)/warning(s)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ;; ("M-s d" . consult-find)
         ("M-s d" . consult-fd)
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
         ;;!("M-e" . consult-isearch-history)        ;; orig. isearch-edit-string
         ;;!("M-s e" . consult-isearch-history)      ;; orig. isearch-edit-string
         ;;!("M-s l" . consult-line)                 ;; needed by consult-line to detect isearch
         ;;!("M-s L" . consult-line-multi)           ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         ;;!:map minibuffer-local-map
         ;;!("M-s" . consult-history)                ;; orig. next-matching-history-element
         ;;!("M-r" . consult-history)                ;; orig. previous-matching-history-element
         )
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init

  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

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
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
  )

(use-package embark
  :bind
  ("C-h B" . embark-bindings)
  :init
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  (if (display-graphic-p)
      (bind-keys
       ("C-." . embark-act)
       ("C-;" . embark-dwim)
       ("C-'" . embark-act-all))
    (bind-keys
     ("C-c ." . embark-act)
     ("C-c ;" . embark-dwim)
     ("C-c '" . embark-act-all)))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-eglot
  :ensure t
  :after eglot)

(use-package consult-eglot-embark
  :ensure t
  :after eglot
  :init
  (consult-eglot-embark-mode))

(use-package company
  :ensure t
  :custom (company-tooltip-align-annotations t)
  :hook (after-init . global-company-mode))

;; modeline
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-support-imenu t)
  :custom
  (doom-modeline-time nil)
  (doom-modeline-project-name t)
  (doom-modeline-buffer-file-name-style 'auto)
  (doom-modeline-hud nil)
  (doom-modeline-icon t)
  (doom-modeline-check-icon nil)
  (doom-modeline-buffer-encoding nil)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (custom-set-faces
   '(doom-modeline-buffer-file ((t (:weight normal))))
   '(doom-modeline-buffer-path ((t (:weight normal))))
   '(doom-modeline-project-dir ((t (:weight normal))))
   '(doom-modeline-project-root-dir ((t (:weight normal))))
   '(doom-modeline-buffer-major-mode ((t (:weight normal))))))

;; theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-material-dark t)
  (custom-set-faces
   '(line-number ((t (:foreground "#757575" :weight normal))))
   '(line-number-current-line ((t (:foreground "#eeffff" :background "#282828" :weight normal))))
   '(font-lock-comment-face ((t (:foreground "#9e9e9e"))))
   '(font-lock-doc-face ((t (:foreground "#9e9e9e"))))
   '(shadow ((t (:foreground "#9e9e9e")))))
  (qu/font-setup))

(use-package golden-ratio
  :disabled
  :ensure t
  :init
  (golden-ratio-mode))

(use-package project
  :config
  (setq project-vc-extra-root-markers '("INSTALL" "COPYING" "LICENSE")))

(use-package symbol-overlay
  :ensure t
  :hook (prog-mode . symbol-overlay-mode)
  :bind (("<f8>" . symbol-overlay-put)
         ("<f9>" . symbol-overlay-remove-all)))

(use-package magit
  :defer t
  :ensure t)

(use-package gptel-magit
  :ensure t
  :after magit
  :config
  (setq gptel-magit-model
        (intern (or (getenv "ANTHROPIC_DEFAULT_HAIKU_MODEL") "deepseek-flash")))
  (setq gptel-magit-backend
        (gptel-make-deepseek "DeepSeek-Commit"
          :key (lambda ()
                 (or (getenv "ANTHROPIC_AUTH_TOKEN")
                     (user-error "Set ANTHROPIC_AUTH_TOKEN for DeepSeek")))
          :models (list gptel-magit-model)
          :request-params '(:thinking (:type "disabled") :max_tokens 1024)))
  (gptel-magit-install))

(use-package blamer
  :ensure t
  :bind (("C-c g b" . blamer-show-posframe-commit-info)
         ("C-c g B" . global-blamer-mode))
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 20)
  (blamer-prettify-time-p t))

;; eglot lsp related
(use-package eglot
  :bind (:map eglot-mode-map
              ("<f2>" . eglot-rename)
              ("C-c e a" . eglot-code-actions)
              ("C-c e f" . eglot-format-buffer))
  :hook
  ((c-mode c-ts-mode) . eglot-ensure)
  ((c++-mode c++-ts-mode) . eglot-ensure)
  ((python-mode python-ts-mode) . eglot-ensure)
  :config
  (setq eglot-autoshutdown t)
  (add-to-list
   'eglot-server-programs
   '((c++-mode c++-ts-mode c-mode c-ts-mode) "ccls"))
  (add-to-list
   'eglot-server-programs
   '((python-mode python-ts-mode) "pylsp")))

(use-package flymake
  :custom
  (flymake-mode-line-lighter "F")
  (flymake-show-diagnostics-at-end-of-line 'short)
  :hook (prog-mode . flymake-mode)
  :bind (("C-M-n" . flymake-goto-next-error)
         ("C-M-p" . flymake-goto-prev-error)
         :map flymake-mode-map
         ("C-c ! b" . flymake-show-buffer-diagnostics)
         ("C-c ! p" . flymake-show-project-diagnostics)))

(use-package treesit
  :if (eq system-type 'gnu/linux)
  :custom
  (treesit-enabled-modes t)
  (treesit-auto-install-grammar 'ask))

(use-package dogears
  :ensure t
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar)))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :custom
  (hs-display-lines-hidden t)
  (hs-show-indicators t)
  :bind (:map hs-minor-mode-map
              ("C-c h" . hs-toggle-hiding)))

;;; Programming
(use-package c-ts-mode
  :config
  (c-ts-mode-set-global-style 'linux)
  :hook
  ((c-ts-mode c++-ts-mode) . (lambda ()
                               (setq-local c-ts-indent-offset 8)
                               (setq-local indent-tabs-mode t)
                               (setq-local tab-width 8))))

(use-package cc-mode
  :hook
  ((c-mode c++-mode) . (lambda ()
                         (c-set-style "linux")
                         (setq-local tab-width 8))))

;;; Latex
(use-package latex
  :defer t
  :ensure auctex
  :hook (LaTeX-mode . cdlatex-mode))

(use-package cdlatex
  :ensure t
  :hook (org-mode . org-cdlatex-mode))

;;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;;; org-mode
(defvar qu/org-root (expand-file-name "~/Documents/Knowing/")
  "Root directory for Org files.")
(setq org-directory qu/org-root)

(defun qu/org-font-setup()
  "Font set for org."
  ;; Title
  (with-eval-after-load 'org-faces
    (set-face-attribute 'org-level-1 nil :height 1.4)
    (set-face-attribute 'org-level-2 nil :height 1.3)
    (set-face-attribute 'org-level-3 nil :height 1.2)
    (set-face-attribute 'org-level-4 nil :height 1.1)
    (set-face-attribute 'org-level-5 nil :height 1.1)
    (set-face-attribute 'org-level-6 nil :height 1.1)
    (set-face-attribute 'org-level-7 nil :height 1.1)
    (set-face-attribute 'org-level-8 nil :height 1.1)))

(use-package org
  :hook
  (org-mode . (lambda ()
                ;;(org-indent-mode)
                ;;(variable-pitch-mode 1) ; variable pitch against fixed-pitch
                (auto-fill-mode 0)
                (visual-line-mode 1)
                (setq-local line-spacing '(0.05 . 0.05))))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :custom
  (org-src-tab-acts-natively t)
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
  (defface qu/org-bold
    '((t (:inherit bold :foreground "#ffcb6b")))
    "Face for bold emphasis in Org buffers."
    :group 'org-faces)
  (setf (cadr (assoc "*" org-emphasis-alist)) 'qu/org-bold)

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

  ;; Org modules
  (require 'org-indent)
  (require 'org-capture)

  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (setq org-todo-keywords '((sequence "TODO(t)" "NOW(n)"
                                      "|"
                                      "DONE(d)" "CANCELED(c)" "FUTURE(f)")))
  (setq org-agenda-custom-commands
        '(("0" "All active tasks" todo "TODO|NOW|FUTURE")
          ("1" "TODO" todo "TODO")
          ("2" "Currently in progress." todo "NOW")
          ("3" "Low priority, maybe later." todo "FUTURE")))

  (setq org-capture-templates
        `(("t" "Todo" entry (file ,(concat org-directory "agenda/tasks.org"))
           "* TODO %?\n  %T\n" :prepend t)
          ("i" "Idea" entry (file ,(concat org-directory "agenda/ideas.org"))
           "* %T\n" :prepend t)))

  (qu/org-font-setup)

  ;; Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)
     (shell . t)
     (python . t)
     (makefile . t)
     (C . t))))

(use-package org-contrib
  :ensure t)

(use-package org-download
  :ensure t
  :after org
  :config
  (setq org-download-method 'directory)
  (setq org-download-image-dir "resource"))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-database-connector 'sqlite-builtin)
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
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

(use-package olivetti
  :ensure t
  :hook (org-mode . olivetti-mode))

(use-package visual-fill-column
  :disabled
  :ensure t
  :hook
  (org-mode . (lambda ()
                (visual-fill-column-mode 1)
                (setq visual-fill-column-width 110
                      visual-fill-column-center-text t)))
  :config
  (setq visual-fill-column-adjust-for-text-scale t)
  (defun qu/visual-fill-column-adjust-if-org ()
    (when (and (derived-mode-p 'org-mode)
               visual-fill-column-mode)
      (visual-fill-column-adjust)))
  (advice-add 'text-scale-adjust :after #'qu/visual-fill-column-adjust-if-org)
  (advice-add 'mouse-wheel-text-scale :after #'qu/visual-fill-column-adjust-if-org))

(use-package valign
  :ensure t
  :hook
  (org-mode . valign-mode))

(use-package adaptive-wrap
  :ensure t
  :hook (org-mode . (lambda () (adaptive-wrap-prefix-mode 1))))

(provide 'init)
;;; init.el ends here
