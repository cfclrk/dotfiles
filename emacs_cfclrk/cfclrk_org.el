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

;;; ox-gfm

(use-package ox-gfm)

;;; Publishing

(require 'ox-publish)

(defun cfclrk/site-preamble (project-plist)
  "Return a string for the HTML preamble.
PROJECT-PLIST has the full contents of all files and properties
in the project."
  (let ((base-directory (plist-get project-plist :base-directory)))
	(f-read (expand-file-name "~/Projects/site/navbar.html"))))

(setq org-publish-project-alist
      `(("site"
		 :components ("cloudformation-org"
					  "cloudformation-html"
                      "cloudformation-static"
					  "notes"
					  "articles"
					  "homepage"
                      "site-static"
                      "site-homepage"))))

;; Add site static and site homepage
;; (use-package site
;;   :straight (:host github :repo "cfclrk/site"))
;; (require 'site)
(load (expand-file-name "~/Projects/site/site.el"))


;; (use-package notes
;;   :straight (:host github :repo "cfclrk/notes"))
;; (require 'notes)
(load (expand-file-name "~/notes/notes.el"))

;; (use-package cloudformation
;;   :straight (:host github :repo "cfclrk/cloudformation"
;;                    :files ("org" :defaults)))
;; (require 'cloudformation)
(load (expand-file-name "~/Projects/cloudformation/cloudformation.el"))


(load (expand-file-name "~/Projects/articles/articles.el"))

;; Add (or update) the site-static project to org-public-project-alist
(setq org-publish-project-alist
      (cons site/org-project-static
            (assoc-delete-all "site-static" org-publish-project-alist)))

;; Add (or update) the projects in site/org-project-alist
(dolist (project site/org-project-alist)
  (let ((project-name (car project)))
    (setq org-publish-project-alist
          (cons project
                (assoc-delete-all project-name org-publish-project-alist)))))

;; Add (or update) the notes project
(setq org-publish-project-alist
      (cons notes/org-project-notes
            (assoc-delete-all "notes" org-publish-project-alist)))

;; Add (or update) the projects in cloudformation/org-projects-alist
(dolist (project cloudformation/org-project-alist)
  (let ((project-name (car project)))
    (setq org-publish-project-alist
          (cons project
                (assoc-delete-all project-name org-publish-project-alist)))))

;; Add (or update) the articles project
(setq org-publish-project-alist
      (cons articles/org-project-articles
            (assoc-delete-all "articles" org-publish-project-alist)))

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
