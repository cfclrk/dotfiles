;;; init.el -- My emacs setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Startup
;;  ----------------------------------------------------------------------------

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name
                       "straight/repos/straight.el/bootstrap.el"
                       user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;; Editor General
;;  ----------------------------------------------------------------------------

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

;; Use a larger font on bigger screens
(when window-system
  (if (> (nth 2 (frame-monitor-attribute 'geometry)) 1600)
      (set-face-attribute 'default nil :height 170)))

;; ⌘ as Meta and ⎇ as Super on MacOS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'hyper)

  ;; Enable emoji
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;;; Modes
;;  ----------------------------------------------------------------------------

;;;; LSP

(use-package lsp-mode
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred)
  :config
  (add-to-list lsp-file-watch-ignored-directories
               "[/\\\\]\\.stonehenge\\'"))

(use-package lsp-ui
  :commands lsp-ui
  :config
  (setq lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-peek-show-directory nil
        lsp-ui-doc-max-height 20))

;;;; Projectile

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode t))

;;; Programming Languages
;;  ----------------------------------------------------------------------------

;;;; Clojure

(use-package clojure-mode
  :hook ((clojure-mode . lsp-deferred)
         (clojure-mode . my-lisp-mode-hook)
         (clojure-mode . cljstyle-format-on-save-mode)))

(use-package cljstyle
  :after clojure-mode
  :straight (cljstyle
             :type git
             :host github
             :repo "cfclrk/cljstyle.el"))

(use-package cider
  :after clojure-mode
  :hook (cider-repl-mode . my-lisp-mode-hook)
  :config
  (setq cider-save-file-on-load t))

(use-package monorepl
  :after cider
  :straight (monorepl
             :local-repo "~/Work/stonehenge"
             :files ("development/emacs/monorepl.el")))

;;;; Lisp

(defun my-lisp-mode-hook ()
  "Configuration common to any LISP."
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

;;;; Clojure

(use-package cljstyle
  :after clojure-mode
  :straight (cljstyle
             :type git
             :host github
             :repo "cfclrk/cljstyle.el"))

;;; Packages/Modes
;;  ----------------------------------------------------------------------------

;;;; rainbow-delimiters

(use-package rainbow-delimiters)

;;;; smartparens

(use-package smartparens
  :bind (:map lisp-mode-map
              ("M-f" . sp-next-sexp)
              ("M-b" . sp-backward-sexp))
  :init
  (require 'smartparens-config)
  :config
  ;; Activates show-smartparens-mode. Turn on visualization of matching pairs.
  (show-smartparens-global-mode t)

  ;; Create a key prefix. I like having a prefix so that which-key can show me
  ;; all the usual actions I perform.
  (global-unset-key (kbd "s-s"))  ; Was an alias for save-buffer, C-x C-s
  (define-prefix-command 'sp-prefix-key-map)
  (define-key smartparens-mode-map (kbd "s-s") sp-prefix-key-map)

  ;; Slurping and barfing with Shift
  (define-key smartparens-mode-map (kbd "S-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "S-<left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-S-<right>") 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-S-<left>") 'sp-backward-slurp-sexp)

  ;; movement
  ;; Maybe M-f and M-b
  ;; Definitely remap C-M-f and C-M-b to use smartparens

  ;; splicing
  (define-prefix-command 'sp-splice-key-map)
  (define-key sp-prefix-key-map (kbd "s") sp-splice-key-map)
  (define-key sp-splice-key-map (kbd "s") 'sp-splice-sexp)
  (define-key sp-splice-key-map (kbd "f") 'sp-splice-sexp-killing-forward)
  (define-key sp-splice-key-map (kbd "b") 'sp-splice-sexp-killing-backward)
  (define-key sp-splice-key-map (kbd "a") 'sp-splice-sexp-killing-around)

  ;; wrapping
  (define-prefix-command 'sp-wrap-key-map)
  (define-key sp-prefix-key-map (kbd "r") sp-wrap-key-map)
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
