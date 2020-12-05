;;; org.el -- Org mode customization  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(prelude-require-packages '(htmlize
                            ob-async
                            org
                            org-bullets))

(require 'org)
(require 'ob-async)
(require 'ob-clojure)
(require 'ox-html)
(require 'ox-org)
(require 'prelude-org)

(setq org-startup-folded t)
(setq org-enforce-todo-dependencies t)

;; Add ox-gfm to load path
(add-to-list 'load-path
             (f-expand "vendor/ox-gfm" (f-canonical user-emacs-directory)))
(require 'ox-gfm)

;;; Functions

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
  "Visit every SRC block and evaluate FN."
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


;;; Hooks

(defun my-org-src-mode-hook ()
  "Customize `org-src-mode' in buffers created by `org-edit-special'."
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (outline-minor-mode nil))

(add-hook 'org-src-mode-hook 'my-org-src-mode-hook)

(defun my-org-mode-hook ()
  "Customize `org-mode'."

  (setq org-src-window-setup 'split-window-below
        org-adapt-indentation nil)

  (org-bullets-mode 1)

  ;; babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (dot . t)
     (python . t)
     (shell . t)))

  ;; (org-babel-lob-ingest (f-join user-emacs-directory
  ;;                               "personal/babel/library-of-babel.org"))

  (setq org-confirm-babel-evaluate nil
        org-babel-clojure-backend 'cider)

  ;; exporting
  (setq org-html-doctype "html5"
        org-html-html5-fancy t
        org-export-with-sub-superscripts nil
        org-html-preamble nil
        org-html-postamble nil
        org-html-head nil)

  ;; publishing
  (require 'ox-publish)
  (setq org-publish-project-alist
        '(("cf-export-org"
           :base-directory "~/Projects/cloudformation/org/"
           :publishing-directory "~/Projects/cloudformation/export_org/"
           :publishing-function org-org-publish-to-org)

          ("cf-export-hugo"
           :base-directory "~/Projects/cloudformation/org/"
           :publishing-directory "~/Projects/cloudformation/export_org/"
           :publishing-function org-org-publish-to-org)

          ("cf-tangle"
           :base-directory "~/Projects/cloudformation/export_org/"
           :publishing-directory "~/Projects/cloudformation/yaml/"
           :publishing-function org-babel-tangle-publish)

          ("cf"
           :components ("cf-export-org" "cf-export-hugo")))))

(add-hook 'org-mode-hook 'my-org-mode-hook)

;;; org.el ends here
