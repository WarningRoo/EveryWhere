;;; early-init.el --- A Practical and Fast Emacs Configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq gc-cons-threshold most-positive-fixnum)
(setq byte-compile-warnings nil)
(setq package-enable-at-startup nil)
(setq load-prefer-newer noninteractive)
(setq frame-inhibit-implied-resize t)
(setq inhibit-startup-screen t)
(setq-default mode-line-format nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(prefer-coding-system 'utf-8)

;;; early-init.el ends here
