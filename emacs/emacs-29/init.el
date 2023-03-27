;;; init.el -- My emacs setup  -*- lexical-binding: t; -*-

;;; Commentary:

;; NOTES:
;;
;; To install a package defined with use-package, evaluate the use-package form
;; and then run (elpaca-process-queues).
;;
;; TODO:
;;
;; - Magit section for team PRs
;; - Magit auto-refresh
;; - precient.el might help with making last-used buffer show up at top of list
;;   when switching buffers
;; - consult?
;; - embark?

;;; Code:

;; elpaca
(load (expand-file-name "bootstraps.el" user-emacs-directory))

;;; Functions
;;  ----------------------------------------------------------------------------

(elpaca nil
  (message "Declaring my functions")

  (defun upsert-alist (quoted-alist entry)
    "Insert or update ENTRY in QUOTED-ALIST.

First, if ENTRY is in QUOTED-ALIST, delete it. Then insert ENTRY.
This prevents duplicates of ENTRY in the alist. Example:

  (setq my-alist '((:foo . 1)))

  (upsert-alist 'my-alist '(:bar . 1))
  ;; => ((:bar . 1) (:foo . 1))

  (upsert-alist 'my-alist '(:foo . 2))
  ;; => ((:foo . 2) (:bar . 1))
"
    (let ((entry-key (car entry))
          (orig-alist (symbol-value quoted-alist)))
      (set quoted-alist
           (cons entry (assoc-delete-all entry-key orig-alist)))))

  (defun my/lsp-remove-all-workspaces ()
    "Clear all LSP workspaces. Sometimes this fixes things."
    (interactive)
    (mapc
     'lsp-workspace-folders-remove
     (lsp-session-folders (lsp-session))))

  (defun my/show-buffer-file-name ()
    "Display and copy the full path to the current file.
  Adapted from Emacs Redux (emacsredux.com)."
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
      (message filename)
      (kill-new filename)))

  (defun my/set-font-size (font-size)
    "Set font height to the given FONT-SIZE.
  This updates font size without changing the Emacs frame (i.e.
  window) size.
  - TODO: display current font size in prompt. You can
  get it with: (face-attribute 'default :height).
  - TODO: Bind to M-F1/M-F2"
    (interactive "nFont Size: ")
    (let ((frame-inhibit-implied-resize t))
      (set-face-attribute 'default nil :height font-size)
      (set-face-attribute 'mode-line nil :height font-size)))

  (defun backward-delete-word (arg)
    "Delete characters backward until encountering the beginning of a word.
  With argument ARG, do this that many times. Unlike
  `backward-kill-word', do not add the word to the `kill-ring'.
  See: https://stackoverflow.com/questions/6133799"
    (interactive "p")
    (delete-region (point) (progn (backward-word arg) (point))))

  (defun pprint (form &optional printcharfun)
    "Return a pretty-printed version of FORM.

  Optional PRINTCHARFUN is as defined by `princ'."
    (princ (with-temp-buffer
             (cl-prettyprint form)
             (buffer-string))
           printcharfun))

  (defun my/font-installed-p (font-name)
    "Check if font with FONT-NAME is available.
  FONT-NAME is a string like 'Roboto Mono'."
    (find-font (font-spec :name font-name))))

(elpaca-wait)

;;; Editor General
;;  ----------------------------------------------------------------------------

;; ⌘ as Meta and ⎇ as Super on MacOS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'hyper))

(tool-bar-mode -1)
(blink-cursor-mode -1)
(global-auto-revert-mode t)  ; Revert buffers when their backing files change
(scroll-bar-mode -1)
(global-hl-line-mode 1)

(setq-default tab-width 4)
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)

(setq make-backup-files nil          ; Don't make those file~ backups
      inhibit-splash-screen t        ; Do not show the welcome screen
      sentence-end-double-space nil
      help-window-select t
      delete-by-moving-to-trash t
      scroll-margin 5)

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

;;; Theme, Font, Display
;;  ----------------------------------------------------------------------------

(load-theme 'modus-operandi)

;; Set the default font to Roboto Mono
(set-face-attribute 'default nil
                    :family "Roboto Mono"
                    :weight 'normal
                    :height 140)

;; Leave left fringe to its default 8px, and set right fringe to 0px
(fringe-mode '(nil . 0))

(use-package all-the-icons
  :config (unless (my/font-installed-p "all-the-icons")
            (all-the-icons-install-fonts t)))

(use-package doom-modeline
  :init (doom-modeline-mode +1)
  :config
  (setq doom-modeline-project-detection 'project
        doom-modeline-buffer-encoding nil
        doom-modeline-height 40
        doom-modeline-hud t
        doom-modeline-project-detection 'projectile
        doom-modeline-buffer-file-name-style 'buffer-name
        doom-modeline-vcs-max-length 15
        doom-modeline-env-version nil))

;;; Text
;;  ----------------------------------------------------------------------------

;; whitespace
(use-package whitespace
  :ensure nil
  :elpaca nil
  :hook (before-save . whitespace-cleanup)
  :custom
  (whitespace-style '(face tabs empty trailing lines-tail))
  (whitespace-line-column nil))

;; text mode

(use-package text-mode
  :ensure nil
  :elpaca nil
  :after (smartparens whitespace)
  :hook ((text-mode . my/text-mode-hook))
  :config
  (defun my/text-mode-hook ()
    "Minor modes that I want enabled in pretty much every textual buffer."
    (smartparens-mode +1)
    (whitespace-mode +1)
    ;; Typed text replaces selection
    (delete-selection-mode +1)))

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

;; (use-package orderless
;;   :custom
;;   (completion-styles '(orderless basic))
;;   (completion-category-overrides '((file (styles basic partial-completion)))))

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
  :config (ctrlf-mode +1))

;;;; code-review

(use-package code-review
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

(define-key global-map (kbd "C-c d") 'dired-jump-other-window)

;; Use coreutils version of ls so I can use the
;; --group-directories-first flag
(setq insert-directory-program "gls"
      dired-use-ls-dired t)

;; Format for listing files
(setq dired-listing-switches "-aoh --group-directories-first")

;; If there is a Dired buffer displayed in some window, use its current
;; directory, instead of this Dired buffer's current directory.
(setq dired-dwim-target t)

;; Reuse current buffer when pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; Always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; Automatically refresh ("revert") Dired buffers
(setq dired-auto-revert-buffer t)

;; In hide-details-mode, still show symlinks
(setq dired-hide-details-hide-symlink-targets nil)

(use-package all-the-icons-dired
  :hook ((dired-mode . all-the-icons-dired-mode))
  :custom
  (all-the-icons-dired-monochrome nil))

;;;; env

(use-package env
  :elpaca (env
           :host github
           :repo "cfclrk/env")
  :config
  (setq env-dir (expand-file-name "~/.env/")))

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
  :config
  (setq magit-diff-refine-hunk 'all)
  :custom
  (magit-save-repository-buffers 'dontask))

;; Credentials are stored in ~/.authinfo
(use-package forge
  :after magit
  :hook (after-save . magit-after-save-refresh-status)
  :config
  (setq forge-owned-accounts '(("cfclrk" . nil)
                               ("cclark-splash" . nil))))

;;;; github-browse-file

(use-package github-browse-file
  :bind ("C-h g" . github-browse-file))

;;;; ispell

(setq ispell-program-name "aspell")

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

(load (expand-file-name "markdown.el" user-emacs-directory))

;;;; occur

;; Always switch focus to the Occur buffer when running occur
(add-hook 'occur-hook
          #'(lambda ()
              (switch-to-buffer-other-window "*Occur*")))

;;;; org

(load (expand-file-name "org.el" user-emacs-directory))

;;;; prog-mode

(add-hook 'prog-mode-hook 'outline-minor-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;;; projectile

(use-package projectile
  :demand t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  ;;(load (expand-file-name "~/emacs/projectile-discovery.el"))
  (projectile-mode +1)
  (setq projectile-use-git-grep t))

;;;; rainbow-delimiters

(use-package rainbow-delimiters)

;;;; recentf

;; Saves recent file names in $user-emacs-directory/recentf. View recent files
;; with C-c f (I configure that keybinding in the crux section).

(use-package recentf
  :ensure nil
  :elpaca nil
  :config
  (setq recentf-max-saved-items 300
	    recentf-max-menu-items 15)
  (recentf-mode +1))

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

;;;; stonehenge

(use-package stonehenge
  :elpaca (stonehenge
           :repo "~/Work/foo"
           :files (:defaults))
  :config
  (customize-set-variable 'stonehenge-dir
                          (expand-file-name "~/Work/stonehenge")))

;;;; terraform

(use-package terraform-mode
  :hook (terraform-mode . lsp-deferred)
  :init (setq lsp-terraform-server '("terraform-ls" "serve")))

;;;; tramp

(setq tramp-default-method "scp")

;;;; undo-tree

(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

;;;; unfill

;; Unfill is the opposite of `fill-paragraph'
(use-package unfill)

;;;; visual-fill-column

(use-package visual-fill-column)

;;;; which-key

(use-package which-key
  :config
  (which-key-mode +1))

;;;; yaml

(use-package yaml-mode)

;;;; yasnippet

;; LSP uses yasnippet to expand snippets. So enabling yas-global-mode is
;; necessary even if you don't load any snippets.

(use-package yasnippet
  :config (yas-global-mode 1))

;;; Programming Languages
;;  ----------------------------------------------------------------------------

;;;; Lisp

(defun my/lisp-mode-hook ()
  "General configuration for any LISP."
  (require 'smartparens)
  (require 'rainbow-delimiters)
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
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
  :hook ((clojure-mode . lsp-deferred)
         (clojure-mode . my/lisp-mode-hook)
         (clojure-mode . cljstyle-format-on-save-mode))
  :bind (:map clojure-mode-map
              ("S-SPC" . just-one-space))
  :custom
  ;; Indent arguments instead of aligning them
  (clojure-indent-style 'always-indent)
  :config
  (setq clojure-build-tool-files (add-to-list
                                  'clojure-build-tool-files
                                  "WORKSPACE")))

(use-package cider
  :after clojure-mode
  :bind (:map cider-mode-map
              ("C-t n" . cider-test-run-ns-tests)
              ("C-t p" . cider-test-run-project-tests)
              ("C-t t" . cider-test-run-test)
              ("C-c C-c" . cider-pprint-eval-defun-at-point)
              ("C-j" . cider-pprint-eval-last-sexp-to-comment))
  :hook ((cider-repl-mode . (lambda () (smartparens-mode +1)))
         (cider-repl-mode . (lambda () (rainbow-delimiters-mode +1))))
  :custom
  ;; Changes how cider-pprint-eval-last-sexp displays things. More here:
  ;; https://docs.cider.mx/cider/usage/pretty_printing.html. Original value was
  ;; nil.
  (cider-print-options '(("length" 50) ("right-margin" 70)))
  ;; Automatically save files before they are loaded in the repl
  (cider-save-file-on-load t)
  ;; Add a newline to the repl prompt
  (cider-repl-prompt-function (lambda (namespace)
                                (format "%s\n> " namespace))))

(use-package cljstyle-format
  :after clojure-mode)

;;;; PHP

(use-package php-mode
  :hook (php-mode . lsp-deferred))
