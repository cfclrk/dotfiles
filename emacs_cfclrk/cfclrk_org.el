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

;;; weblorg

(use-package weblorg)

;;; Publishing

(defun cfclrk/site-preamble (project-plist)
  "Return a string for the HTML preamble.
PROJECT-PLIST has the full contents of all files and properties
in the project."
  (let ((base-directory (plist-get project-plist :base-directory)))
	(f-read (expand-file-name "../html_partials/navbar.html" base-directory))))

(defun cfclrk/compile-scss (project-plist)
  "Compile SCSS to CSS. PROJECT-PLIST has all project info."
  (let* ((base-directory (plist-get project-plist :base-directory))
		 (scss-file (expand-file-name "main.scss" base-directory))
		 (out-file (expand-file-name "main.css" base-directory)))
	(call-process "sass" nil nil nil scss-file out-file)))

(defun cfclrk/sitemap-function (title org-list)
  "How to create and format the sitemap.
TITLE is the sitmap title. ORG-LIST is as returned by
`org-list-parse-list'."
  (message (pp org-list))
  (org-list-to-subtree org-list))

(defun cfclrk/sitemap-format-entry (entry style project)
  "Defines how a sitemap entry is formatted.
TODO: For articles, add date and abstract."
  "foo")

(setq cfclrk/html-head
	  (f-read (expand-file-name "~/Projects/technotes/html_partials/head.html")))

(require 'ox-publish)
(setq org-publish-project-alist
	  `(("org"
         :recursive t
		 :base-directory "~/Projects/technotes/org"
		 :publishing-directory "~/Projects/technotes/_gen"
		 :publishing-function org-org-publish-to-org
		 :auto-sitemap t
		 :sitemap-function cfclrk/sitemap-function
		 :sitemay-sort-files anti-chronologically)

		("cf-tangle"
         :recursive t
		 :base-directory "~/Projects/technotes/_gen/cloudformation"
		 :publishing-directory "~/Projects/technotes/_site/tangle/cloudformation"
		 :publishing-function org-babel-tangle-publish
		 :exclude "parameters\\.org")

		("html"
		 :recursive t
		 :base-directory "~/Projects/technotes/_gen"
		 :publishing-directory "~/Projects/technotes/_site"
		 :publishing-function org-html-publish-to-html
		 :exclude "setup\\.org"
		 :html-head-include-scripts nil
		 :html-head-include-default-style nil
         :with-creator nil
		 :with-author nil
		 :html-head ,cfclrk/html-head,
		 :section-numbers nil
		 :html-preamble cfclrk/site-preamble
		 :html-self-link-headlines t)

		("static"
		 :recursive t
		 :base-directory "~/Projects/technotes/static"
		 :publishing-directory "~/Projects/technotes/_site/static"
		 :base-extension "png\\|jpg\\|gif\\|pdf\\|css"
		 :publishing-function org-publish-attachment
		 :preparation-function cfclrk/compile-scss)

		("site"
		 :components ("org"
					  "cf-tangle"
					  "html"
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
  (setq org-html-checkbox-type 'html
		org-html-doctype "html5"
		org-html-html5-fancy t
		org-html-postamble nil
		org-html-validation-link nil))

(add-hook 'org-mode-hook 'cfclrk/org-mode-hook)

(provide 'cfclrk-org)
;;; cfclrk_org.el ends here
