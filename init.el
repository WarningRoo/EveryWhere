;;; init.el -- init.el

;;; Commentary:
;; Less is more

;;; Code:

;; Require
(require 'cl-lib)

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
  "Font setup."
  ;; Set default font
  (cl-loop for font in '("SF Mono" "MesloLGS NF" "Jetbrains Mono" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute 'default nil :font (font-spec :family font
                                                                    :weight 'Regular
                                                                    :size 18)))

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

;; Open FILE as root
(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":"
                         (file-remote-p file 'user) "@" (file-remote-p file 'host)
                         "|sudo:root@"
                         (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))

;;; BASIC
(setq inhibit-startup-screen t)
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
(setq-default indent-tabs-mode nil)
(set-input-method 'TeX)
(setq use-dialog-box nil)
(setq tab-width 4)
(setq c-ts-mode-indent-offset 4)
(set-frame-parameter nil 'alpha 1.00)

;; auto-revert
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

;; Date & Time
(display-time)
(setq display-time-day-and-date t)

;; Display the column indicator
(setq-default fill-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;;; line/column number
(column-number-mode 1)
;; Enable line numbers for some modes
(dolist (mode '(prog-mode-hook
                diff-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'before-save-hook (lambda () (whitespace-cleanup)))

;;; Keybindings
(global-set-key (kbd "C-c '") 'comment-or-uncomment-region)
(global-set-key (kbd "M-o") 'other-window)

;; Pixel scroll
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)
(defalias 'scroll-up-command 'pixel-scroll-interpolate-down)
(defalias 'scroll-down-command 'pixel-scroll-interpolate-up)

;; Scroll line
(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-n") 'scroll-up-line)

;; move window
;; C-S-<right/left/up/down>
(windmove-swap-states-default-keybindings '(ctrl shift))

;; ABOUT Package
(eval-when-compile (require 'use-package))
(use-package package
  :config
  (setq use-package-always-ensure t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (unless (bound-and-true-p package--initialized)
    (package-initialize)))

(use-package benchmark-init
  :disabled
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"           "Home")
     ("d" "~/Downloads/" "Downloads")
     ("c" "~/Documents/" "Documents")
     ("r" "~/Repo/"      "Repo")))
  :config
  (dirvish-side-follow-mode)
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("<f8>"  . dirvish-side)
   ("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(use-package hl-line
  :hook
  (after-init . global-hl-line-mode)
  :config
  (setq hl-line-range-function (lambda ()
                                 (cons (line-end-position)
                                       (line-beginning-position 2)))))

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

(use-package copilot-chat
  :defer t
  :config
  (setq copilot-chat-frontend 'org))

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

(use-package exec-path-from-shell
  :after eshell
  :config
  (exec-path-from-shell-copy-env "ARCH")
  (exec-path-from-shell-copy-env "CROSS_COMPILE")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package nerd-icons)

(use-package dashboard
  :custom
  (dashboard-center-content t)
  ;; Agenda
  (dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-todo)
  (dashboard-match-agenda-entry "+TODO=\"NOW\"")
  (dashboard-agenda-sort-strategy '(priority-down))
  (dashboard-agenda-prefix-format " ")
  ;; Homepage
  (dashboard-items '((recents . 10)
                     (bookmarks . 10)
                     (agenda . 10)))
  (dashboard-startupify-list '(dashboard-insert-items))
  :config
  (dashboard-setup-startup-hook))

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

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
  :bind  (("C-." . embark-act)
          ("C-;" . embark-dwim)
          ("C-'" . embark-act-all)
          ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
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
(load-theme 'modus-vivendi-tinted t)

(use-package doom-modeline
  :init
  (setq doom-modeline-support-imenu t)
  :custom
  (doom-modeline-project-name t)
  (doom-modeline-hud t)
  :hook
  (after-init . doom-modeline-mode))

(use-package project
  :config
  (setq project-vc-extra-root-markers '("INSTALL" "COPYING" "LICENSE")))

(use-package highlight-symbol
  :bind ("<f9>" . highlight-symbol))

(use-package magit
  :defer t)

;; eglot lsp related
(use-package eglot
  :hook
  ((c-mode c-ts-mode) . eglot-ensure)
  ((c++-mode c++-ts-mode) . eglot-ensure)
  ((lisp-mode emacs-lisp-mode) . eglot-ensure)
  ((python-mode python-ts-mode) . eglot-ensure)
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
   '((python-mode python-ts-mode) "pylsp")
   ))

(use-package flymake
  :custom
  (flymake-mode-line-lighter "F")
  :hook (prog-mode . flymake-mode)
  :bind (("C-M-n" . flymake-goto-next-error)
         ("C-M-p" . flymake-goto-prev-error)))

(use-package treesit-auto
  :if (eq system-type 'gnu/linux)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package dogears
  :bind
  (:map global-map
        ("M-g d" . dogears-go)
        ("M-g M-b" . dogears-back)
        ("M-g M-f" . dogears-forward)
        ("M-g M-d" . dogears-list)
        ("M-g M-D" . dogears-sidebar)))

;;; LaTeX
(use-package latex
  :defer t
  :ensure auctex)

(use-package cdlatex)

;;; Markdown
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

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
    (set-face-attribute 'org-level-8 nil :height 1.1)))

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
        '(("0" "All needed to to" todo "TODO|NOW|FUTURE")
          ("1" "TODO" todo "TODO")
          ("2" "Currently in progress." todo "NOW")
          ("3" "Low priority, maybe later." todo "FUTURE")))

  (setq org-capture-templates
        `(("t" "Todo" entry (file ,(concat org-directory "agenda/tasks.org")) "* TODO %?\n  %T\n" :prepend t)
          ("i" "Idea" entry (file ,(concat org-directory "agenda/ideas.org")) "* %T\n" :prepend t)))

  (qu/org-font-setup)

  ;; require Programming Languages Support
  (require 'ob-lisp)
  (require 'ob-shell)
  (require 'ob-python)
  (require 'ob-makefile))

(use-package org-contrib)

(use-package org-roam
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

(use-package valign
  :hook
  (org-mode . valign-mode))

(use-package adaptive-wrap
  :hook (org-mode . (lambda () (adaptive-wrap-prefix-mode 1))))

(provide 'init)
;;; init.el ends here
