;;; init.el -- Minimimal org + clojure emacs setup  -*- lexical-binding: t; -*-

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

;;; Early init - Org Mode
;;  ----------------------------------------------------------------------------

;; Prevent loading the built-in org-mode. Instead, use straight to get org-mode.
;; We must run this before anything else loads the built-in org-mode. Straight
;; gets org-mode from the mirror here: https://github.com/emacs-straight
(use-package org)

;;; Editor General
;;  ----------------------------------------------------------------------------

(blink-cursor-mode -1)
(set-language-environment "UTF-8")

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default fill-column 80)

(setq inhibit-splash-screen t ;; Do not show the welcome page
      make-backup-files nil)  ;; Do not save ~ backup files

;; ⌘ as Meta and ⎇ as Super on MacOS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'hyper))

;;; Packages/Modes
;;  ----------------------------------------------------------------------------

;;;; clojure

(use-package clojure-mode)

(use-package cider
  :after clojure-mode)

;;;; org

(org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (emacs-lisp . t)))

(setq org-adapt-indentation t
      org-confirm-babel-evaluate nil)

;;; init.el ends here
