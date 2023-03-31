;;; org.el -- Org mode config  -*- lexical-binding: t; -*-

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
  :custom
  (org-startup-folded t)
  (org-confirm-babel-evaluate nil)
  (org-adapt-indentation t)
  (org-src-window-setup 'split-window-below)
  (org-special-ctrl-a/e t)
  (org-babel-min-lines-for-block-output 40)
  (org-hide-leading-stars t))
