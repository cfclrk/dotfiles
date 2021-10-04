;;; org.el -- Org mode customization  -*- lexical-binding: t; -*-

;;; Commentary:

;; My customization for org-mode.

;;; Code:

;;; Packages
;;  ----------------------------------------------------------------------------

(prelude-require-packages '(htmlize
                            ob-async
                            org
                            org-bullets))

;; Add ox-gfm to load path
(add-to-list 'load-path
             (f-expand "vendor/ox-gfm" (f-canonical user-emacs-directory)))

(require 'org)
(require 'ob-async)
(require 'prelude-org)

;; I believe these must be specified before the org-mode-hook is run?
(setq org-startup-folded t)
(setq org-enforce-todo-dependencies t)

;;; Functions
;;  ----------------------------------------------------------------------------

(defun cfc/org-md ()
  "Export an org file to GitHub Flavored Markdown and format."
  (interactive)
  (let* ((org-file-name (buffer-file-name))
         (md-file-name (f-swap-ext org-file-name "md")))

    ;; Export org to GitHub Flavored Markdown
    (org-export-to-file 'gfm md-file-name)

    ;; Format the markdown
    (with-temp-buffer
      (insert-file-contents md-file-name)
      (markdown-mode)
      (let ((fill-column 80))
        (fill-region (point-min) (point-max)))
      (write-file md-file-name))))

(defun cfc/on-every-src-block (fn)
  "Visit every source block and evaluate FN."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^\s*#[+]BEGIN_SRC" nil t)
        (let ((element (org-element-at-point)))
          (when (eq (org-element-type element) 'src-block)
            (funcall fn element)))))
    (save-buffer)))

(defun cfc/org-remove-results ()
  "Remove all RESULTS blocks in an org file."
  (interactive)
  (cfc/on-every-src-block 'org-babel-remove-result))

;;; Presentations
;;  ----------------------------------------------------------------------------

(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))


;;; Publishing
;;  ----------------------------------------------------------------------------

(require 'ox-publish)
(setq org-publish-project-alist
      '(("export-org"
         :recursive t
         :base-directory "~/Projects/cloudformation/org/"
         :publishing-directory "~/Projects/cloudformation/exported_org/"
         :publishing-function org-org-publish-to-org)

        ("tangle-cf"
         :recursive t
         :base-directory "~/Projects/cloudformation/exported_org/"
         :publishing-directory "~/Projects/cloudformation/tangled_cf/"
         :publishing-function org-babel-tangle-publish)

        ("cf"
         :components ("export-org" "tangle-cf"))))

;;; Hooks
;;  ----------------------------------------------------------------------------

(defun my-org-src-mode-hook ()
  "Customize `org-src-mode' in buffers created by `org-edit-special'."
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (outline-minor-mode nil))
(add-hook 'org-src-mode-hook 'my-org-src-mode-hook)


(require 'ob-clojure)
(require 'ob-shell)
(defun my-org-mode-hook ()
  "Customize `org-mode'."

  (setq org-src-window-setup 'split-window-below
        org-adapt-indentation nil)

  (auto-fill-mode 1)
  (org-bullets-mode 1)

  ;; Don't highlight lines longer than fill-column
  (whitespace-toggle-options '(lines-tail))

  ;; Babel languages to load
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (dot . t)
     (emacs-lisp . t)
     (plantuml . t)
     (python . t)
     (shell . t)))

  ;; Set :noweb header to yes by default
  (setq org-babel-default-header-args
        (cons '(:noweb . "yes")
              (assq-delete-all :noweb org-babel-default-header-args)))

  ;; Ensure incorrect shell blocks fail nicely
  (add-to-list 'org-babel-default-header-args:sh
               '(:prologue . "set -eu -o pipefail"))
  (add-to-list 'org-babel-default-header-args:bash
               '(:prologue . "set -eu -o pipefail"))

  ;; Load Library of Babel functions
  (org-babel-lob-ingest
   (f-join user-emacs-directory "personal/library-of-babel.org"))

  (setq org-confirm-babel-evaluate nil
        org-babel-clojure-backend 'cider)

  ;; Exporting
  (require 'ox-gfm)
  (require 'ox-html)
  (require 'ox-org)
  (setq org-html-doctype "html5"
        org-html-html5-fancy t
        org-export-with-sub-superscripts nil
        org-html-preamble nil
        org-html-postamble nil
        org-html-head nil))

(add-hook 'org-mode-hook 'my-org-mode-hook)

;;; org.el ends here
