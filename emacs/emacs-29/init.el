;;; init.el -- My emacs setup  -*- lexical-binding: t; -*-

;;; Commentary:

;; NOTES:
;;
;; To install a package defined with use-package, evaluate the use-package form
;; and then run (elpaca-process-queues).
;;
;; TODO:
;;
;; - forge View Topic mode - q should kill buffer (maybe winner-undo)
;; - visual-line-mode in magit diff buffers (to wrap long lines in diff)
;; - Create jira link in forge topic buffers
;;  - Would be cool if I could hover over a ticket number and get the description
;; - gh-notify
;;   - gh-notify-copy-url is an API URL that can't be used in the browser
;;   - merged state should be the merged emoji
;;   - open state could be green circle
;; - C-x c to go to *Code Review* buffer if it exists
;; - Magit section for team PRs
;; - Magit auto-refresh?
;; - Cape?
;; - precient.el might help with making last-used buffer show up at top of list
;;   when switching buffers
;; - consult?
;; - embark?

;;; Code:

(load (expand-file-name "init-bootstraps.el" user-emacs-directory))

(elpaca-wait)

;; Load dash, s, and f so that I can use them in my own functions.
(use-package dash
  :config
  ;; Fontify dash-defined anaphoric vars ("it", "acc", etc)
  (global-dash-fontify-mode)

  ;; Enable C-h S (info-lookup-symbol) on dash symbols
  (with-eval-after-load 'info-look
    (dash-register-info-lookup)))

(use-package s)
(use-package f)

(elpaca-wait)

(load (expand-file-name "init-functions.el" user-emacs-directory))

;;; Editor General
;;  ----------------------------------------------------------------------------

;; ⌘ as Meta and ⎇ as Super on MacOS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'hyper))

(tool-bar-mode -1)
(blink-cursor-mode -1)
(global-auto-revert-mode t)
(scroll-bar-mode -1)
(global-hl-line-mode 1)

;; See: https://github.com/doomemacs/doomemacs/blob/e47accb77324ffff6e9a72bf820a0f73d2e804f3/lisp/doom-start.el#L134-L137
(set-language-environment "UTF-8")
(setq default-input-method nil)

(setq-default tab-width 4)
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)

(setq make-backup-files nil
      inhibit-splash-screen t
      sentence-end-double-space nil
      help-window-select t
      delete-by-moving-to-trash t
      scroll-margin 0)

;; The mode line
(column-number-mode t)
(size-indication-mode t)

;; Change all yes/no prompts to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Make shell scripts executable when saved
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Keep customizations outside of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(add-hook 'elpaca-after-init-hook
          (lambda () (load custom-file 'noerror)))

;; A .zsh file is a shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; Make nicer keybinding for `xref-find-definitions-other-window'
(define-key global-map
            (kbd "C-c M-.")
            'xref-find-definitions-other-window)

;; C-<backspace> was backward-kill-word. That pollutes kill ring.
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)

;; C-c z to see full path of file in the current buffer
(global-set-key (kbd "C-c z") 'my/show-buffer-file-name)

;; Clean up global-map
(load (expand-file-name "clean-global-map.el" user-emacs-directory))

;;; Theme, Font, Display
;;  ----------------------------------------------------------------------------

(load-theme 'leuven)

;; Set the default font to Roboto Mono
(set-face-attribute 'default nil
                    :family "Roboto Mono"
                    :weight 'normal
                    :height 120)

(setq-default line-spacing 0.2)

;; Leave left fringe to its default (8px), and set right fringe to 0px
(fringe-mode '(nil . 0))

(use-package all-the-icons
  :config (unless (my/font-installed-p "all-the-icons")
            (all-the-icons-install-fonts t)))

(use-package doom-modeline
  :init (doom-modeline-mode)
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-env-version nil)
  (doom-modeline-hud t)
  (doom-modeline-project-detection 'projectile)
  (doom-modeline-vcs-max-length 15))

;;; Text
;;  ----------------------------------------------------------------------------

;; whitespace
(use-package whitespace
  :ensure nil
  :elpaca nil
  :hook (before-save . whitespace-cleanup)
  :custom
  (whitespace-style '(face tabs empty trailing))
  (whitespace-line-column nil))

;; text mode

(use-package text-mode
  :ensure nil
  :elpaca nil
  :after (smartparens whitespace)
  :hook ((text-mode . my/text-mode-hook))
  :config
  (defun my/text-mode-hook ()
    (smartparens-mode)
    (whitespace-mode)
    (delete-selection-mode)))

;;; Completion
;;  ----------------------------------------------------------------------------

(use-package prescient
  :elpaca (prescient
           :files ("*.el")))

(use-package vertico
  :after prescient
  :elpaca (vertico
           :files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  (vertico-prescient-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :elpaca nil
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

(use-package marginalia
  :after vertico
  :init (marginalia-mode))

(use-package corfu
  :after prescient
  :hook (corfu-mode . corfu-prescient-mode)
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-scroll-margin 5))

(use-package cape)

(use-package consult
  :bind (:map global-map
              ("M-g g" . consult-goto-line)))

;;; Packages/Modes
;;  ----------------------------------------------------------------------------

;;;; ace

(use-package ace-window
  :demand t
  :bind ("M-l" . ace-window)
  :config (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

;;;; bazel

(use-package bazel
  :elpaca (bazel
           :host github
           :repo "bazelbuild/emacs-bazel-mode")
  :custom
  (bazel-buildifier-before-save t))

;;;; bicycle

(use-package bicycle
  :demand t
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle)
              ([S-tab] . bicycle-cycle-global)))

;;;; ctrlf

(use-package ctrlf
  :config (ctrlf-mode))

;;;; code-review

;; Using branch until this issue is addressed:
;; https://github.com/wandersoncferreira/code-review/issues/245
(use-package code-review
  :elpaca (code-review
           :host github
           :repo "phelrine/code-review"
           :branch "fix/closql-update"
           :depth nil)
  :custom
  (code-review-auth-login-marker 'forge))

;;;; crux

(use-package crux
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c f" . crux-recentf-find-file)))

;;;; diff-hl

(use-package diff-hl
  :demand t
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode))

;;;; dired

(use-package dired
  :ensure nil
  :elpaca nil
  :bind (:map global-map
              ("C-c d" . dired-jump-other-window))
  :config
  ;; Reuse current buffer when pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)
  :custom
  ;; If there is a Dired buffer displayed in some window, use its current
  ;; directory, instead of this Dired buffer's current directory.
  (dired-dwim-target t)

  ;; Automatically refresh ("revert") Dired buffers
  (dired-auto-revert-buffer t)

  ;; Format for listing files
  (dired-listing-switches "-aoh --group-directories-first")

  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)

  ;; In hide-details-mode, still show symlinks
  (dired-hide-details-hide-symlink-targets nil)

  ;; Pass the "--dired" option to "ls"
  (dired-use-ls-dired t))

;; Use coreutils version of ls so I can use the
;; --group-directories-first flag
(setq insert-directory-program "gls")

(use-package nerd-icons-dired
  :hook ((dired-mode . nerd-icons-dired-mode)))

;;;; ejc-sql

(use-package ejc-sql
  :elpaca (ejc-sql
           :depth nil))

;;;; env

(use-package environ
  :elpaca (environ
           :host github
           :repo "cfclrk/environ"
           :depth nil)
  :config
  (setq environ-dir (expand-file-name "~/.env/")))

;;;; fish

(use-package fish-mode
  :config
  (add-to-list 'completion-at-point-functions 'cape-file))

;;;; flycheck

(use-package flycheck
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode))

;;;; helpful

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h C" . helpful-command)))

;;;; git, magit, forge

(use-package git-modes
  :config
  ;; gitconfig-mode
  (add-to-list 'auto-mode-alist '("\\.gitconfig\\'" . gitconfig-mode))
  ;; gitignore-mode
  (add-to-list 'auto-mode-alist '("/.dockerignore\\'" . gitignore-mode))
  (add-to-list 'auto-mode-alist '("CODEOWNERS\\'" . gitignore-mode)))

(use-package magit
  :bind (:map magit-diff-mode-map
              ("<C-return>" . magit-diff-visit-file-other-window)
              ("<M-return>" . magit-diff-visit-worktree-file-other-window))
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-save-repository-buffers 'dontask))

;; Credentials are stored in ~/.authinfo
(use-package forge
  :after magit
  :hook (after-save . magit-after-save-refresh-status)
  :config
  (setq forge-owned-accounts '(("cfclrk" . nil)
                               ("cclark-splash" . nil))))

;;;; gh-notify

(use-package gh-notify)

;;;; github-browse-file

(use-package github-browse-file
  :bind ("C-h g" . github-browse-file))

;;;; ispell

(setq ispell-program-name "aspell")

;;;; jsonian

;; Better json
(use-package jsonian)

;;;; key-chord

(use-package key-chord
  :config
  (key-chord-define-global "\\a" 'treemacs-select-window)
  (key-chord-define-global "\\o" 'treemacs)
  (key-chord-mode +1))

;;;; LSP

(use-package lsp-mode
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-enable-file-watchers nil))

(use-package lsp-ui
  :commands lsp-ui
  :custom
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-symbol nil)
  (lsp-ui-doc-max-height 20))

;;;; markdown

(load (expand-file-name "init-markdown.el" user-emacs-directory))

;;;; occur

;; Always switch focus to the Occur buffer when running occur
(add-hook 'occur-hook
          #'(lambda ()
              (switch-to-buffer-other-window "*Occur*")))

;;;; org

(load (expand-file-name "init-org.el" user-emacs-directory))

;;;; prog-mode

(add-hook 'prog-mode-hook 'outline-minor-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;;; projectile

(use-package projectile
  :demand t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (setq projectile-project-root-files-bottom-up
        (-concat
         '("go.mod")
         projectile-project-root-files-bottom-up
         projectile-project-root-files))
  :custom
  (projectile-use-git-grep t))

;;;; rainbow-delimiters

(use-package rainbow-delimiters)

;;;; recentf

;; Saves recent file names in $user-emacs-directory/recentf. View recent files
;; with C-c f (I configure that keybinding in the crux section).

(use-package recentf
  :ensure nil
  :elpaca nil
  :config
  (recentf-mode)
  :custom
  (recentf-max-saved-items 300)
  (recentf-max-menu-items 15))

;;;; rg

;; Library for Emacs to use ripgrep. Projectile can use this.
(use-package rg)

;;;; smartparens

(use-package smartparens
  :init (require 'smartparens-config)
  :bind (:map lisp-mode-map
              ("M-f" . sp-next-sexp)
              ("M-b" . sp-backward-sexp))
  :custom
  (sp-wrap-repeat-last 0)
  :config
  (load (expand-file-name "~/emacs/smartparens.el"))
  (my-smartparens-config))

;;;; spacious-padding

(use-package spacious-padding
  :elpaca (spacious-padding
           :host github
           :repo "protesilaos/spacious-padding")
  :config
  (spacious-padding-mode))

;;;; terraform

(use-package terraform-mode
  :hook (terraform-mode . lsp)
  :init (setq lsp-terraform-server '("terraform-ls" "serve")))

;;;; tramp

(setq tramp-default-method "scp")

;;;; treemacs

(use-package treemacs
  :hook (treemacs-mode . (lambda () (treemacs-load-theme "all-the-icons"))))

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons))

(use-package lsp-treemacs
  :init
  (defun my/lsp-treemacs-symbols-toggle ()
    "Toggle the lsp-treemacs-symbols buffer."
    (interactive)
    (let ((buffer-name "*LSP Symbols List*"))
      (if (get-buffer buffer-name)
          (kill-buffer buffer-name)
        (progn
          (lsp-treemacs-symbols)
          (other-window -1)))))
  :custom
  (lsp-treemacs-theme "all-the-icons"))

;;;; undo-tree

(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history t))

;;;; unfill

;; Unfill is the opposite of `fill-paragraph'
(use-package unfill)

;;;; visual-fill-column

(use-package visual-fill-column
  :custom
  ;; Fix a problem where visual-fill-column cuts lines early
  (visual-fill-column-extra-text-width '(1 . 1)))

;;;; which-key

(use-package which-key
  :config
  (which-key-mode))

;;;; winner

(winner-mode)

;;;; yaml

(use-package yaml-mode)

;;;; yasnippet

;; LSP uses yasnippet to expand snippets. So enabling yas-global-mode is
;; necessary even if you don't load any snippets.

(use-package yasnippet
  :config (yas-global-mode))

;;; Programming Languages
;;  ----------------------------------------------------------------------------

;;;; Lisp

(defun my/lisp-mode-hook ()
  (require 'smartparens)
  (require 'rainbow-delimiters)
  (smartparens-strict-mode)
  (rainbow-delimiters-mode)
  (setq fill-column 80)
  ;; Restart whitespace mode so that it properly uses `fill-column'.
  (whitespace-mode -1)
  (whitespace-mode +1))

;;;; Emacs Lisp

(use-package elisp-mode
  :ensure nil
  :elpaca nil
  :hook (emacs-lisp-mode . my/lisp-mode-hook)
  :bind ("C-c C-k" . eval-buffer))

;;;; Clojure

(use-package clojure-mode
  :mode "\\.cljstyle\\'"  ; Use clojure-mode for ".cljstyle" files
  :hook ((clojure-mode . lsp)
         (clojure-mode . my/lisp-mode-hook)
         (clojure-mode . cljstyle-format-on-save-mode))
  :bind (:map clojure-mode-map
              ("S-SPC" . just-one-space))
  :custom
  ;; Indent arguments instead of aligning them
  (clojure-indent-style 'always-indent)
  (clojure-toplevel-inside-comment-form t))

(use-package cider
  :after clojure-mode
  :bind (:map cider-mode-map
              ("C-t n" . cider-test-run-ns-tests)
              ("C-t p" . cider-test-run-project-tests)
              ("C-t t" . cider-test-run-test)
              ("C-c C-c" . cider-pprint-eval-defun-at-point)
              ("C-j" . cider-pprint-eval-last-sexp-to-comment))
  :hook ((cider-repl-mode . (lambda () (smartparens-mode)))
         (cider-repl-mode . (lambda () (rainbow-delimiters-mode))))
  :custom
  ;; Changes how cider-pprint-eval-last-sexp displays things. More here:
  ;; https://docs.cider.mx/cider/usage/pretty_printing.html.
  (cider-print-fn 'zprint)
  ;; Automatically save files before they are loaded in the repl
  (cider-save-file-on-load t)
  ;; Add a newline to the repl prompt
  (cider-repl-prompt-function (lambda (namespace)
                                (format "%s\n> " namespace))))

(use-package cljstyle-format
  :after clojure-mode)

;;;; CSS

(use-package css-mode
  :ensure nil
  :elpaca nil
  :hook (css-mode . lsp)
  :config
  ;; This should probably be project-local.
  (setq lsp-css-experimental-custom-data
      '("/Users/cclark/Projects/codenotes/css/lsp_tailwind_custom_data.json"))
  :custom
  css-indent-offset 2)

;;;; PHP

(use-package php-mode
  :hook ((php-mode . lsp)
         (php-mode . smartparens-strict-mode)))

(use-package phpunit
  :after php-mode
  :bind (:map php-mode-map
              ("C-t c" . phpunit-current-class)
              ("C-t t" . phpunit-current-test)))

;;;; Python

(defun my/python-mode-hook ()
  "Customize `python-mode'."
  (setq fill-column 88
        python-fill-docstring-style 'pep-257-nn
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i")

  ;; Restart whitespace-mode so that it properly uses `fill-column'
  (whitespace-mode -1)
  (whitespace-mode +1))

(use-package python-mode
  :ensure nil
  :elpaca nil
  :hook (python-mode . my/python-mode-hook))

;; LSP using the pyright language server
(use-package lsp-pyright
  :init (setq lsp-pyright-multi-root nil)
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; Code formatter
(use-package python-black
  :after python)

;; Sort imports
(use-package python-isort
  :after python)

;;;; SQL

;; This doesn't work, even though "sqlfluff format --dialect snowflake
;; <file>.sql" does. Need to figure this out.
(use-package sqlformat
  :config
  (setq sqlformat-command 'sqlfluff
        sqlformat-args '("--dialect"
                         "snowflake")))

;;; Work

(use-package splash
  :elpaca (splash :repo "~/Work/stonehenge"
                  :depth 1
                  :files ("development/emacs/splash.el"
                          "splash/chris/development/emacs/splash-env.el"))
  :custom
  (splash-stonehenge-dir "/Users/cclark/Work/stonehenge")
  (splash-website-dir "/Users/cclark/Work/Website"))
