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

(defun cfclrk/site-preamble (project-plist)
  "Return a string for the HTML preamble.
PROJECT-PLIST has the full contents of all files and properties
in the project."
  (let ((base-directory (plist-get project-plist :base-directory)))
	(f-read (expand-file-name "~/Projects/site/navbar.html"))))

(setq org-publish-project-alist
      `(("articles"
         :recursive t
		 :base-directory "~/Projects/articles"
		 :publishing-directory "~/Projects/cfclrk.com/articles"
		 :publishing-function org-html-publish-to-html
		 :exclude "setup\\.org"
		 :auto-sitemap t
		 ;; :sitemap-function cfclrk/sitemap-function
		 :sitemap-sort-files anti-chronologically
		 :sitemap-title "Articles"
         :html-head-include-scripts nil
		 :html-head-include-default-style nil
         :with-creator nil
		 :with-author nil
		 :section-numbers nil
		 :html-preamble cfclrk/site-preamble
		 :html-self-link-headlines t
		 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/main.css\" />")

		("site"
		 :components ("cf-org"
					  "cf-html"
					  "notes"
					  "articles"
					  "homepage"
                      "site-static"
                      "site-homepage"))))

;; Add site static and site homepage
(use-package site
  :straight (:host github :repo "cfclrk/site"))
;; (load (expand-file-name "~/Projects/site/site.el"))
(require 'site)

(use-package notes
  :straight (:host github :repo "cfclrk/notes"))
;; (load (expand-file-name "~/notes/notes.el"))

(use-package cloudformation
  :straight (:host github :repo "cfclrk/cloudformation"))
;; (load (expand-file-name "~/Projects/cloudformation/cloudformation.el"))

;; Add (or update) the site-static project to org-public-project-alist
(setq org-publish-project-alist
      (cons site/org-project-static
            (assoc-delete-all "site-static" org-publish-project-alist)))

;; Add (or update) the site-homepage project
(setq org-publish-project-alist
      (cons site/org-project-homepage
            (assoc-delete-all "site-homepage" org-publish-project-alist)))

;; Add (or update) the notes project
(setq org-publish-project-alist
      (cons notes/org-project-notes
            (assoc-delete-all "notes" org-publish-project-alist)))

;; Add (or update) the cloudformation-org project
(setq org-publish-project-alist
      (cons cloudformation/org-project-cloudformation-org
            (assoc-delete-all "cloudformation-org" org-publish-project-alist)))

;; Add (or update) the cloudformation-html project
(setq org-publish-project-alist
      (cons cloudformation/org-project-cloudformation-html
            (assoc-delete-all "cloudformation-html" org-publish-project-alist)))

;; Add (or update) the cloudformation-static project
(setq org-publish-project-alist
      (cons cloudformation/org-project-cloudformation-static
            (assoc-delete-all "cloudformation-static" org-publish-project-alist)))

;; (require 'cfclrk-site)
;; (cons org-publish-project-alist cfclrk-site/static)

;; To remove a project:
;; (assoc-delete-all "notes" org-publish-project-alist)

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
