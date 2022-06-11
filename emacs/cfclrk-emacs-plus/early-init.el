;;; early-init.el --- Early Initialization. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Prevent GC from running at all. Reset it later at the end of init.el.
(setq gc-cons-percentage-original gc-cons-percentage
      gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(setq tool-bar-mode nil
      menu-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;;; early-init.el ends here
