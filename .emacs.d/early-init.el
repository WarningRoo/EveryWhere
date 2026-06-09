;;; early-init.el --- A Practical and Fast Emacs Configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq gc-cons-threshold most-positive-fixnum)
(setq byte-compile-warnings nil)
(setq package-enable-at-startup nil)
(setq load-prefer-newer noninteractive)
(setq inhibit-startup-screen t)

;; Frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(prefer-coding-system 'utf-8)

;;; early-init.el ends here
