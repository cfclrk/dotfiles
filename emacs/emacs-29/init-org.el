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
