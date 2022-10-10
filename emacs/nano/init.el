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

(setq
 ;; Make every use-package declaration use straight. This also accomplishes what
 ;; `use-package-always-ensure' does.
 straight-use-package-by-default t
 ;; Make straight use ssh instead of https
 straight-vc-git-default-protocol 'ssh)

;;; Nano
;;  ----------------------------------------------------------------------------

(straight-use-package
 '(nano-emacs
   :type git
   :host github
   :repo "rougier/nano-emacs"))

(straight-use-package
 '(nano-theme :type git :host github
              :repo "rougier/nano-theme"))

(require 'nano)
(nano-theme)

;;; Editor General
;;  ----------------------------------------------------------------------------

(blink-cursor-mode -1)
(set-language-environment "UTF-8")

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

;;; packages/modes
;;  ----------------------------------------------------------------------------

;;;; ace

(use-package ace-window
  :bind ("M-l" . ace-window)
  :config (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

;;;; vertico

(use-package vertico
  :init
  (vertico-mode))

;;;; markdown-xwidget

(use-package markdown-xwidget
  :straight (markdown-xwidget
             :type git
             :host github
             :repo "cfclrk/markdown-xwidget"
             :files (:defaults "resources"))
  :config
  (setq markdown-xwidget-github-theme "dark-dimmed"
        markdown-xwidget-mermaid-theme "dark"
        markdown-xwidget-code-block-theme "github-dark-dimmed"))

;;; init.el ends here
