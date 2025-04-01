;;; early-init.el --- A Practical and Fast Emacs Configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)
(setq load-prefer-newer noninteractive)
(prefer-coding-system 'utf-8)
(setq frame-inhibit-implied-resize t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq-default mode-line-format nil)

;;; early-init.el ends here
