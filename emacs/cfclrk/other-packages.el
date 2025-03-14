;;; other-packages.el -- Config for packages I don't init -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A dumping ground for config I used for a package one time or another. I don't
;; load these packages from init.el.

;;; Code:

;;;; forge

;; Credentials are stored in ~/.authinfo
(use-package forge
  :after magit
  :hook (after-save . magit-after-save-refresh-status)
  :config
  (setq forge-owned-accounts '(("cfclrk" . nil)
                               ("cclark-splash" . nil))))

;;;; web-mode

;; For laravel blade templates.
(use-package web-mode
  :config
  ;; For some reason this takes over php files
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
  ;; Don't indent the contents of the <html> tag
  ;; (setq web-mode-offsetless-elements '("html"))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-sql-indent-offset 2)

  ;; How much to indent CSS inside of a <style> tag
  (web-mode-style-padding 2))
