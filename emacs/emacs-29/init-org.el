;;; init-org.el -- Org mode config  -*- lexical-binding: t; -*-

;;; Commentary:

;; My customization for org-mode.

;;; Code:

(use-package org
  :elpaca nil
  :ensure nil
  :hook ((org-mode . my/org-mode-hook)
         (org-mode . smartparens-mode))
  :config
  (defun my/org-mode-hook ()
    (auto-fill-mode 1))

  (setq org-file-apps
        '((auto-mode . emacs)
          (directory . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . default)
          ("\\.crt\\'" . emacs)
          (t . emacs)))

  ;;Exporting
  (require 'ox)
  (setq org-html-checkbox-type 'html
        org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-postamble nil
        org-html-validation-link nil
        ;; Prevent timestamps from being inserted in generated HTML
        org-export-time-stamp-file nil)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (dot . t)
     (emacs-lisp . t)
     (js . t)
     (python . t)
     (shell . t)
     (sql . t)))

  ;;; Publishing
  (let ((site.el (expand-file-name "~/Projects/cfclrk.com/site.el")))
    (when (f-exists? site.el)
      (progn
        (load site.el)
        (let ((notes.el (expand-file-name "notes/notes.el" site/project-directory))
              (articles.el (expand-file-name "articles/articles.el" site/project-directory))
              (cloudformation.el (expand-file-name "~/Projects/cloudformation/cloudformation.el")))
          (load notes.el)
          (load articles.el)
          (when (f-exists? cloudformation.el)
            (load cloudformation.el))))))
  :custom
  (org-startup-folded t)
  (org-confirm-babel-evaluate nil)
  (org-adapt-indentation t)
  (org-src-window-setup 'split-window-below)
  (org-special-ctrl-a/e t)
  (org-babel-min-lines-for-block-output 40)
  (org-hide-leading-stars t))

;;; Functions

(defun org-outline-tempdir (&optional empty)
  "Create a temporary directory for the current outline section.

If EMPTY is non-nil, deletes the contents of the directory first.

The directory is created relative to
`variable:temporary-file-directory', at:

    org-outline/<file-name>/<heading 1>/<heading 2>/...

Returns the directory name."
  (interactive)
  (let ((outline-path (org-get-outline-path 'with-self))
        (doc-path (list temporary-file-directory
                        "org-outline"
                        (f-base (buffer-file-name)))))

    (let ((tempdir (file-name-as-directory
                    (apply #'f-join (append doc-path outline-path)))))

      ;; Clear the directory if the "empty" param is given
      (when empty
        (delete-directory tempdir 'recursive)
        (make-directory tempdir))

      ;; Return the temp dir name.
      tempdir)))

(defun org-outline-tempdir-dired ()
  "Open Dired in a temporary directory for this outline section."
  (dired (make-directory (org-outline-tempdir) 'parents)))

(provide 'init-org)
;;; init-org.el ends here
