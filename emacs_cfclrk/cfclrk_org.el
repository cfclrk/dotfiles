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
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-hide-leading-stars t))

;;; org-tree-slide

(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))

;;; Publishing

(require 'ox-publish)
(setq org-publish-project-alist
	  '(("articles"
         :recursive t
		 :base-directory "~/Projects/technotes/articles"
		 :publishing-directory "~/Projects/technotes/_site/articles"
		 :publishing-function org-html-publish-to-html
		 :html-head-include-scripts nil
		 :html-head-include-default-style nil
		 :auto-sitemap t)

		("notes"
		 :recursive t
		 :base-directory "~/Projects/technotes/notes"
		 :publishing-directory "~/Projects/technotes/_site/notes"
		 :publishing-function org-html-publish-to-html
		 :html-head-include-scripts nil
		 :html-head-include-default-style nil
		 :auto-sitemap t)

		("cf-include"
         :recursive t
		 :base-directory "~/Projects/technotes/cloudformation/org"
		 :publishing-directory "~/Projects/technotes/cloudformation/_gen"
		 :publishing-function org-org-publish-to-org)

		("cf-tangle"
         :recursive t
		 :base-directory "~/Projects/technotes/cloudformation/_gen"
		 :publishing-directory "~/Projects/technotes/cloudformation/_tangle"
		 :publishing-function org-babel-tangle-publish)

		("cf-html"
         :recursive t
		 :base-directory "~/Projects/technotes/cloudformation/_gen"
		 :publishing-directory "~/Projects/technotes/_site/cloudformation"
		 :publishing-function org-html-publish-to-html
		 :html-head-include-scripts nil
		 :html-head-include-default-style nil
		 :auto-sitemap t)

		("static"
		 :recursive t
		 :base-directory "~/Projects/technotes/static"
		 :publishing-directory "~/Projects/technotes/_site/static"
		 :base-extension "png\\|jpg\\|gif\\|pdf\\|css"
		 :publishing-function org-publish-attachment)

		("site"
		 :components ("articles"
					  "notes"
					  "cf-include"
					  "cf-tangle"
					  "cf-html"
					  "static"))))

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
		org-special-ctrl-a/e t
		org-babel-clojure-backend 'cider)

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

  ;; Set babel default headrs
  (add-to-list 'org-babel-default-header-args '(:noweb . "yes"))
  (add-to-list 'org-babel-default-header-args '(:eval . "never-export"))

  ;; Ensure incorrect shell blocks fail nicely
  (add-to-list 'org-babel-default-header-args:sh
               '(:prologue . "set -eu -o pipefail"))
  (add-to-list 'org-babel-default-header-args:bash
               '(:prologue . "set -eu -o pipefail"))

  ;; HTML exporting
  (setq org-html-doctype "html5"
		org-html-html5-fancy t
		org-html-postamble nil
		org-html-validation-link nil))

(add-hook 'org-mode-hook 'cfclrk/org-mode-hook)

(provide 'cfclrk-org)
;;; cfclrk_org.el ends here
