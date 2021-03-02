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
	(f-read (expand-file-name "~/Projects/site/navbar.html"))))

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
  ;;(message (pp org-list))
  (org-list-to-subtree org-list))

(setq cfclrk/html-head
	  (f-read (expand-file-name "~/Projects/technotes/html_partials/head.html")))

(defun cfclrk/org-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let* ((xml-declaration (plist-get info :html-xml-declaration))
	    (decl (or (and (stringp xml-declaration) xml-declaration)
		      (cdr (assoc (plist-get info :html-extension)
				  xml-declaration))
		      (cdr (assoc "html" xml-declaration))
		      "")))
       (when (not (or (not decl) (string= "" decl)))
	 (format "%s\n"
		 (format decl
			 (or (and org-html-coding-system
				  (fboundp 'coding-system-get)
				  (coding-system-get org-html-coding-system 'mime-charset))
			     "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
	   (cond ((org-html-xhtml-p info)
		  (format
		   " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
		   (plist-get info :language) (plist-get info :language)))
		 ((org-html-html5-p info)
		  (format " lang=\"%s\"" (plist-get info :language))))
	   ">\n")
   "<head>\n"
   (org-html--build-meta-info info)
   (org-html--build-head info)
   (org-html--build-mathjax-config info)
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
	 (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format (plist-get info :html-home/up-format)
	       (or link-up link-home)
	       (or link-home link-up))))
   ;; Preamble.
   (org-html--build-pre/postamble 'preamble info)
   ;; Document contents.
   (let ((div (assq 'content (plist-get info :html-divs))))
     (format "<%s id=\"%s\">\n" (nth 1 div) (nth 2 div)))
   ;; Document title.
   (when (plist-get info :with-title)
     (let ((title (and (plist-get info :with-title)
		       (plist-get info :title)))
	   (subtitle (plist-get info :subtitle))
	   (html5-fancy (org-html--html5-fancy-p info)))
       (when title
	 (format
	  (if html5-fancy
	      "<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
	    "<h1 class=\"title\">%s%s</h1>\n")
	  (org-export-data title info)
	  (if subtitle
	      (format
	       (if html5-fancy
		   "<p class=\"subtitle\">%s</p>\n"
		 (concat "\n" (org-html-close-tag "br" nil info) "\n"
			 "<span class=\"subtitle\">%s</span>\n"))
	       (org-export-data subtitle info))
	    "")))))
   contents
   (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)
   ;; Possibly use the Klipse library live code blocks.
   (when (plist-get info :html-klipsify-src)
     (concat "<script>" (plist-get info :html-klipse-selection-script)
	     "</script><script src=\""
	     org-html-klipse-js
	     "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
	     org-html-klipse-css "\"/>"))
   ;; Closing document.
   "</body>\n</html>"))

(org-export-define-derived-backend 'cfclrk-html 'html
  :translate-alist '((template . cfclrk/html-template)))

(require 'ox-publish)
(setq org-publish-project-alist

	  ;; Move this to cloudformation repo. Then in this file, I include that
	  ;; file by pulling it straight from github (just how straight does it).
	  ;; Each project can define its own backend, if it wants.
	  `(("cf-org"
         :recursive t
		 :base-directory "~/Projects/cloudformation/org"
		 :publishing-directory "~/Projects/cloudformation/_org"
         :publishing-function org-org-publish-to-org
		 :auto-sitemap t
		 :sitemap-title "CloudFormation")

		("cf-html"
         :recursive t
		 :base-directory "~/Projects/cloudformation/_org"
		 :publishing-directory "~/Projects/cfclrk.com/cloudformation"
		 :publishing-function (org-babel-tangle-publish
							   org-html-publish-to-html)
		 :exclude "params\\.org"
		 :auto-sitemap t
         :html-head-include-scripts nil
		 :html-head-include-default-style nil
         :with-creator nil
		 :with-author nil
		 :section-numbers nil
		 :html-preamble cfclrk/site-preamble
		 :html-self-link-headlines t
		 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/main.css\" />")

		("cf-static"
		 :recursive t
		 :base-directory "~/Projects/cloudformation/static"
		 :publishing-directory "~/Projects/cfclrk.com/static"
		 :base-extension "png\\|jpg\\|gif\\|pdf\\|css"
		 :publishing-function org-publish-attachment)

		("notes"
         :recursive t
		 :base-directory "~/notes"
		 :publishing-directory "~/Projects/cfclrk.com/notes"
		 :publishing-function org-html-publish-to-html
		 :exclude "setup\\.org"
		 :auto-sitemap t
		 :sitemap-title "Notes"
		 ;; :sitemap-function cfclrk/sitemap-function
         :html-head-include-scripts nil
		 :html-head-include-default-style nil
         :with-creator nil
		 :with-author nil
		 :section-numbers nil
		 :html-preamble cfclrk/site-preamble
		 :html-self-link-headlines t
		 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/main.css\" />")

		("articles"
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

		("homepage"
		 :base-directory "~/Projects/site"
		 :publishing-directory "~/Projects/cfclrk.com"
		 :publishing-function org-html-publish-to-html
		 :base-extension "org"
         :html-head-include-scripts nil
		 :html-head-include-default-style nil
         :with-creator nil
		 :with-author nil
		 :section-numbers nil
		 :html-preamble cfclrk/site-preamble
		 :html-self-link-headlines t
		 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/static/main.css\" />")

        ("static"
		 :recursive t
		 :base-directory "~/Projects/site/static"
		 :publishing-directory "~/Projects/cfclrk.com/static"
		 :base-extension "png\\|jpg\\|gif\\|pdf\\|css"
		 :publishing-function org-publish-attachment
		 :preparation-function cfclrk/compile-scss)

		("site"
		 :components ("cf-org"
					  "cf-html"
					  "notes"
					  "articles"
					  "homepage"
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
