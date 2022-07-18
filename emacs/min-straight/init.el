;;; init.el -- My emacs setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Straight
;;  ----------------------------------------------------------------------------

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name
                       "straight/repos/straight.el/bootstrap.el"
                       user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;; Editor General
;;  ----------------------------------------------------------------------------

(blink-cursor-mode -1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default fill-column 80)

(setq help-window-select t    ;; Always select the help window
      inhibit-splash-screen t ;; Do not show the welcome page
      make-backup-files nil)  ;; Do not save ~ backup files

;; Use a larger font on bigger screens
(set-face-attribute 'default nil :height 170)

;; ⌘ as Meta and ⎇ as Super on MacOS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'hyper))

;;; Packages/Modes
;;  ----------------------------------------------------------------------------

;;;; ace

(use-package ace-window
  :bind ("M-l" . ace-window)
  :config (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

;;;; vertico

(use-package vertico
  :init
  (vertico-mode))

;;;;; markdown-defaults

(use-package markdown-defaults
  :straight (markdown-defaults
             :type git
             :host github
             :repo "cfclrk/markdown-defaults"
             :files (:defaults "resources"))
  :config
  (setq markdown-defaults-github-theme "light-high-contrast"
        markdown-defaults-mermaid-theme "neutral"
        markdown-defaults-code-block-theme "github"))

;;; init.el ends here
