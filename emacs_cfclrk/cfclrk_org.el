;;; cfclrk_org.el -- Org mode customization  -*- lexical-binding: t; -*-

;;; Commentary:

;; My customization for org-mode.

;;; Code:

;; TODO: Try out https://github.com/Fuco1/org-radiobutton

(require 'org)

;;; htmlize

(use-package htmlize)

;;; org-superstar

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

;;; org-tree-slide

(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))

;;; Publishing

(require 'ox-publish)
(setq org-publish-project-alist
	  '(("technotes"
		 :recursive t
		 :base-directory "~/Projects/technotes/org/"
		 :publishing-directory "~/Projects/technotes/html/"
		 :publishing-function org-html-publish-to-html)))

;;; org-src mode

(defun cfclrk/org-src-mode-hook ()
  "Customize `org-src-mode' in buffers created by `org-edit-special'."
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (outline-minor-mode nil))

(add-hook 'org-src-mode-hook 'cfclrk/org-src-mode-hook)

;;; org mode

(defun cfclrk/org-mode-hook ()
  "Customize `org-mode'."
  (turn-on-auto-fill)

  (setq org-startup-folded t
		org-confirm-babel-evaluate nil
		org-src-window-setup 'split-window-below
		org-special-ctrl-a/e t)

  ;; Note my smartparens config also pulls in 'smartparens-org
  (smartparens-mode +1)

  ;; Babel languages to load
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (dot . t)
     (emacs-lisp . t)
	 (gnuplot . t)
	 (python . t)
     (shell . t)))

  ;; HTML exporting
  (setq org-html-doctype "html5"
		org-html-html5-fancy t
		org-html-postamble nil
		org-html-validation-link nil))

(add-hook 'org-mode-hook 'cfclrk/org-mode-hook)

(provide 'cfclrk-org)
;;; cfclrk_org.el ends here
