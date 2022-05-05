;;; init.el -- My emacs setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/") ; http instead of https
        ("melpa" . "https://melpa.org/packages/")))

(blink-cursor-mode -1)
(set-language-environment "UTF-8")

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default fill-column 80)

(setq column-number-mode t    ;; Show line:column in mode line
      help-window-select t
      inhibit-splash-screen t ;; Do not show the welcome page
      make-backup-files nil)  ;; Do not save ~ backup files

;; Keep customizations outside of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; ⌘ as Meta and ⎇ as Super on MacOS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'hyper)

  ;; Enable emoji
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(use-package cljstyle
  :after clojure-mode
  :straight (cljstyle
             :type git
             :host github
             :repo "cfclrk/cljstyle.el"))

(use-package clojure-mode
  :hook ((clojure-mode . cljstyle-format-on-save-mode)))

;;; Packages/Modes
;;  ----------------------------------------------------------------------------

;;;; ace

(use-package ace-window
  :bind ("M-l" . ace-window)
  :config (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

;;;; bicycle

(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle)
              ([S-tab] . bicycle-cycle-global)))

(add-hook 'prog-mode-hook 'outline-minor-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;;;; rainbow-delimiters

(use-package rainbow-delimiters)

;;;; smartparens

(use-package smartparens
  :bind (:map smartparens-mode-map
              ;; slurping and barfing
              ("S-<right>" . sp-forward-slurp-sexp)
              ("S-<left>" . sp-forward-barf-sexp)
              ("M-S-<right>" . sp-backward-barf-sexp)
              ("M-S-<left>" . sp-backward-slurp-sexp))
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode t)

  ;; Create a key prefix M-p
  (define-prefix-command 'sp-prefix-key-map)
  (define-key smartparens-mode-map (kbd "M-p") sp-prefix-key-map)

  ;; splicing prefix is M-p s
  (define-prefix-command 'sp-splice-key-map)
  (define-key sp-prefix-key-map (kbd "s") sp-splice-key-map)

  ;; splicing commands
  (define-key sp-splice-key-map (kbd "s") 'sp-splice-sexp)
  (define-key sp-splice-key-map (kbd "f") 'sp-splice-sexp-killing-forward)
  (define-key sp-splice-key-map (kbd "b") 'sp-splice-sexp-killing-backward)
  (define-key sp-splice-key-map (kbd "a") 'sp-splice-sexp-killing-around)

  ;; wrapping prefix is M-p r
  (define-prefix-command 'sp-wrap-key-map)
  (define-key sp-prefix-key-map (kbd "r") sp-wrap-key-map)

  ;; wrapping commands
  (define-key sp-wrap-key-map (kbd "a") 'sp-wrap-round) ; mneumonic: "around"
  (define-key sp-wrap-key-map (kbd "u") 'sp-unwrap-sexp)
  (define-key sp-wrap-key-map (kbd "c") 'sp-wrap-curly)
  (define-key sp-wrap-key-map (kbd "r") 'sp-rewrap-sexp)

  ;; selection
  (define-key sp-prefix-key-map (kbd "n") 'sp-select-next-thing)
  (define-key sp-prefix-key-map (kbd "p") 'sp-select-previous-thing-exchange))

;;;; undo-tree

(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

;;;; yaml

(use-package yaml-mode)

;;; init.el ends here
