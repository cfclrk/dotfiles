;;; org.el -- Org mode customization  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(prelude-require-packages '(htmlize
                            org
                            org-bullets
                            ox-gfm))

(require 'org)
(require 'ob-clojure)
(require 'ox-gfm)
(require 'ox-html)
(require 'prelude-org)

(setq org-startup-folded t)
(setq org-enforce-todo-dependencies t)

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

  (org-babel-lob-ingest (f-join user-emacs-directory
                                "babel/library-of-babel.org"))

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

        '(("Blog"
           :base-directory "~/Projects/jekyll/blog/org/posts/"
           :publishing-directory "~/Projects/jekyll/blog/_posts/"
           base-extension "org"
           :publishing-function org-html-publish-to-html
           :body-only t)

          ("orgtests"
           :base-directory "~/Luminal/orgtests/org/"
           :publishing-directory "~/Luminal/orgtests/dist/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :body-only t)))

  (defun cfc/wrap-with-vue-template ()
    (with-temp-buffer
      (insert-file-contents "~/cat.txt")
      (write-region
       (concat "<template><div>" (buffer-string) "</div></template>") nil "~/cat2.txt")))

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
  )

(add-hook 'org-mode-hook 'my-org-mode-hook)
