;;; init-org.el -- Org mode config  -*- lexical-binding: t; -*-

;;; Commentary:

;; My customization for org-mode.

;;; Code:

(require 's)

;; htmlize is required to provide syntax highlighting in published source blocks
;; (i.e. HTML code blocks generating from `org-publish-project').
(use-package htmlize)

(use-package org
  :ensure nil
  :hook ((org-mode . my/org-mode-hook)
         (org-mode . smartparens-mode))
  :config
  (defun my/org-mode-hook ()
    (auto-fill-mode 1))

  ;; TODO: This removes org-cycle-show-empty-lines from the hook. Does this
  ;; accomplish anything? It doesn't work to remove the newline after a heading
  ;; when pressing TAB.
  (setq org-cycle-hook
        '(org-cycle-hide-archived-subtrees
          org-cycle-optimize-window-after-visibility-change
          org-cycle-display-inline-images))

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
     (mermaid . t)
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

(use-package ob-mermaid
  :ensure (ob-mermaid
           :host github
           :depth nil
           :repo "cfclrk/ob-mermaid"))

;;; Functions

(defun org-outline-tempdir (&optional empty)
  "Create a temporary directory for the current outline section.

If EMPTY is non-nil, deletes the contents of the directory first.

The directory is created relative to
`variable:temporary-file-directory', at:

    org-outline/<file-name>/<heading 1>/<heading 2>/...

Returns the directory name.

TODO: look into function org-attach-dir."
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

(defun my/on-every-src-block (fn)
  "Visit every source block and evaluate FN."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^\s*#[+]BEGIN_SRC" nil t)
        (let ((element (org-element-at-point)))
          (when (eq (org-element-type element) 'src-block)
            (funcall fn element)))))
    (save-buffer)))

(defun my/org-remove-results ()
  "Remove all RESULTS blocks in an org file."
  (interactive)
  (my/on-every-src-block 'org-babel-remove-result))

(defun host (user ip path &optional sudo)
  "Return a TRAMP string for SSHing to a remote host.
USER is a user name on the remote host IP. PATH is the path on
the remote host at which to execute the source block. If SUDO is
non-nil, use sudo on the remote host."
  (if sudo
      (s-lex-format "/ssh:${user}@${ip}|sudo:${ip}:${path}")
    (s-lex-format "/ssh:${user}@${ip}:${path}")))

(provide 'init-org)
;;; init-org.el ends here
