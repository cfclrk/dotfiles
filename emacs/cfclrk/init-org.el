;;; init-org.el -- Org mode config  -*- lexical-binding: t; -*-

;;; Commentary:

;; My customization for org-mode.

;;; Code:

(require 'org)

;;; General

(setq org-file-apps
      '((auto-mode . emacs)
        (directory . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . default)
        ("\\.crt\\'" . emacs)
        (t . emacs)))

;;; Packages

;;;; htmlize

(use-package htmlize)

;;;; ob-async

(use-package ob-async)

;;;; ob-http

(use-package ob-http)

;;;; ob-mermaid

(use-package ob-mermaid
  :straight (ob-mermaid
             :host github
             :repo "arnm/ob-mermaid"
             :fork (:host github
                    :repo "cfclrk/ob-mermaid")))

;;;; org-contrib

(use-package org-contrib)

;;;; org-superstar

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-hide-leading-stars t))

;;;; ox-gfm

(use-package ox-gfm)

;;;; ox-slack

(use-package ox-slack)

;;; Functions

(defun org-outline-tempdir (&optional empty)
  "Create a temporary directory for the current outline section.

If EMPTY is non-nil, empty the directory first.

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
      (if empty
          (delete-directory tempdir 'recursive))

      ;; Return the temp dir name.
      tempdir)))


(defun org-outline-tempdir-dired ()
  "Open Dired in a temporary directory for this outline section."
  (dired (make-directory (org-outline-tempdir) 'parents)))


(defun cfclrk/org-md ()
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

(defun cfclrk/on-every-src-block (fn)
  "Visit every source block and evaluate FN."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^\s*#[+]BEGIN_SRC" nil t)
        (let ((element (org-element-at-point)))
          (when (eq (org-element-type element) 'src-block)
            (funcall fn element)))))
    (save-buffer)))

(defun cfclrk/org-remove-results ()
  "Remove all RESULTS blocks in an org file."
  (interactive)
  (cfclrk/on-every-src-block 'org-babel-remove-result))

(defun host (user ip path &optional sudo)
  "Return a TRAMP string for SSHing to a remote host.
USER is a user name on the remote host IP. PATH is the path on
the remote host at which to execute the source block. If SUDO is
non-nil, use sudo on the remote host."
  (if sudo
      (s-lex-format "/ssh:${user}@${ip}|sudo:${ip}:${path}")
    (s-lex-format "/ssh:${user}@${ip}:${path}")))

;;; Agenda

(setq org-agenda-files '("~/Work/notes/todo.org"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(n)" "|" "DONE(d)")))

;;; Babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (http . t)
   (js . t)
   (mermaid . t)
   (python . t)
   (shell . t)
   (sql . t)))

;;; Publishing

;; Load my projects
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

;;; Hooks

;;;; org-src mode hook

(defun cfclrk/org-src-mode-hook ()
  "Customize `org-src-mode' in buffers created by `org-edit-special'."
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (outline-minor-mode nil))

(add-hook 'org-src-mode-hook 'cfclrk/org-src-mode-hook)

;;;; org mode hook

(defun cfclrk/org-mode-hook ()
  "Customize `org-mode'."
  (turn-on-auto-fill)

  (setq org-startup-folded t
        org-confirm-babel-evaluate nil
        org-adapt-indentation t
        org-src-window-setup 'split-window-below
        org-special-ctrl-a/e t
        org-babel-clojure-backend 'cider
        org-babel-min-lines-for-block-output 40)

  ;; Note: This smartparens config also pulls in 'smartparens-org
  (smartparens-mode +1)

  ;; Unset some keybindings
  (local-unset-key (kbd "C-c [")) ; `org-agenda-file-to-front'
  (local-unset-key (kbd "C-c ]")) ; `org-remove-file'
  (local-unset-key (kbd "C-'"))   ; `org-cycle-agenda-files'
  (local-unset-key (kbd "C-,"))   ; `org-cycle-agenda-files'

  ;; Babel default header arguments
  (upsert-alist 'org-babel-default-header-args '(:noweb . "yes"))
  (upsert-alist 'org-babel-default-header-args '(:exports . "both"))
  (upsert-alist 'org-babel-default-header-args '(:eval . "never-export"))

  ;; Ensure incorrect shell blocks fail nicely TODO: This causes "set -eu -o
  ;; pipefail" to be inserted before every block in tangled files!
  ;; (upsert-alist 'org-babel-default-header-args:sh
  ;;               '(:prologue . "set -e -o pipefail"))
  ;; (upsert-alist 'org-babel-default-header-args:bash
  ;;               '(:prologue . "set -e -o pipefail"))

  ;; HTML exporting
  (setq org-html-checkbox-type 'html
        org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-postamble nil
        org-html-validation-link nil
        ;; Prevent timestamps from being inserted in generated HTML
        org-export-time-stamp-file nil))

(add-hook 'org-mode-hook 'cfclrk/org-mode-hook)

(provide 'init-org)
;;; init-org.el ends here
