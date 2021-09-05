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

;; Number of bytes that can be read from a sub-process in one read operation.
;; Good for dealing with verbose subprocesses, like *ehem* an LSP server.
(setq read-process-output-max (* 4 1024 1024)) ;; 4 MiB (default is 8 KiB)

;; Amount of memory allowed before garbage collection. If you set this
;; too high, GC takes a long time.
(setq gc-cons-threshold (* 3 1024 1024))  ;; 3 MiB (default is 800 KB)

;; Log a message about startup time
(defun cfclrk/startup-hook ()
  "Show me some Emacs startup stats."
  (interactive)
  (message "*** Emacs loaded in %s with %d garbage collections."
           (format "%.1f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook 'cfclrk/startup-hook)

;;; Functions
;;  ----------------------------------------------------------------------------

;; At the top because I use dash in my own functions.

(use-package dash
  :config
  ;; Fontify dash-defined anaphoric vars ("it", "acc", etc)
  (global-dash-fontify-mode)

  ;; Enable C-h S (info-lookup-symbol) on dash symbols
  (with-eval-after-load 'info-look
    (dash-register-info-lookup)))

(defun any-file-in-dir? (file-list dir)
  "True if any of the files in FILE-LIST is in the directory DIR.
Otherwise false."
  (--any (f-exists? (f-expand it dir)) file-list))

(defun upsert-alist (quoted-alist entry)
  "Insert or update ENTRY in QUOTED-ALIST.
This first deletes the existing item if it is there, then enserts
the new ENTRY.

Example (backslashes not necessary, FFS Elisp!):

    (upsert-alist '(:noweb . \"yes\") 'org-babel-default-header-args)

Without the backslashes. Elisp strings suck."
  (let ((keyword (car entry))
        (orig-alist (symbol-value quoted-alist)))
    (set quoted-alist
         (cons entry (assoc-delete-all keyword orig-alist)))))

(defun pprint (form &optional printcharfun)
  "Return a pretty-printed version of FORM.

Optional PRINTCHARFUN is as defined by `princ'."
  (princ (with-temp-buffer
           (cl-prettyprint form)
           (buffer-string))
         printcharfun))

(defun cfclrk/show-buffer-file-name ()
  "Display and copy the full path to the current file.
Adapted from Emacs Redux (emacsredux.com)."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (message filename)
    (kill-new filename)))

;; C-c z to see full path of file in the current buffer
(global-set-key (kbd "C-c z") 'cfclrk/show-buffer-file-name)

(defun cfclrk/set-font-size (font-size)
  "Set font height to the given FONT-SIZE.
TODO: display current font size in prompt. You can get it
with: (face-attribute 'default :height)."
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

(global-set-key (kbd "C-<backspace>") 'backward-delete-word)

;;; Theme, Font, Display
;;  ----------------------------------------------------------------------------

;; Use Source Code Pro on MacOS
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Source Code Pro"))

;; Use a larger font on big monitors
(when window-system
  (if (> (nth 2 (frame-monitor-attribute 'geometry)) 1600)
      (set-face-attribute 'default nil :height 170)))

;; Use the doom-one theme
(use-package doom-themes
  :config
  (setq doom-modeline-project-detection 'project
		doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(defun cfclrk/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(use-package all-the-icons
  :config (unless (cfclrk/font-installed-p "all-the-icons")
			(all-the-icons-install-fonts t)))

(use-package doom-modeline
 :init (doom-modeline-mode +1)
 :config
 (setq doom-modeline-buffer-encoding nil)
 (setq doom-modeline-height 40))

(setq display-buffer-alist
      '(("\\*eshell\\*" display-buffer-use-some-window)))

;;; Editor General
;;  ----------------------------------------------------------------------------

(tool-bar-mode -1)    ;; No tool bar, which has the save button, etc
(scroll-bar-mode -1)  ;; No scroll bars to the right of buffers
(blink-cursor-mode -1)  ;; Just a nice, solid cursor
(global-auto-revert-mode t)  ;; Revert buffers when their backing files change
(set-language-environment "UTF-8")

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default fill-column 80)

(setq column-number-mode t    ;; show line:column in mode line
      make-backup-files nil
      inhibit-splash-screen t
      sentence-end-double-space nil
      help-window-select t)

;; whitespace
(require 'whitespace)
(setq whitespace-style '(face tabs empty trailing lines-tail)
      whitespace-line-column nil)
(add-hook 'before-save-hook 'whitespace-cleanup)

(defun cfclrk/text-editing-hook ()
  "Minor modes that I want enabled in pretty much every textual buffer."
  (smartparens-mode +1)
  (whitespace-mode +1)
  (delete-selection-mode +1))

;; text mode
(add-hook 'text-mode-hook 'cfclrk/text-editing-hook)

;; Make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; A .zsh file is a shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; This is nice in go-mode. Should I only do this in go-mode?
(define-key global-map (kbd "C-c M-.") 'xref-find-definitions-other-window)

;; Change all yes/no prompts to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Keep customizations outside of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Remap C-h g from `describe-gnu-project' to `github-browse-file'
(global-set-key (kbd "C-h g") 'github-browse-file)

;; MacOS
(when (eq system-type 'darwin)
  ;; ⌘ as Meta and ⎇ as Super
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'hyper)

  ;; Enable emoji
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)

  ;; Set the PATH env var as we set it in the shell
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

;;;; Registers

(setq register-preview-delay 0)

;;; Packages/Modes
;;  ----------------------------------------------------------------------------

;;;; ace

(use-package ace-window
  :bind ("M-l" . ace-window)
  :config (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

;;;; beacon

;; Beacon is a package that temporarily highlights or flashes the line that the
;; cursor is on when switching buffers.

(use-package beacon
  :config
  (beacon-mode +1)
  (setq beacon-color "#a9a1e1"))

;;;; bicycle

(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle)
              ([S-tab] . bicycle-cycle-global)))

(add-hook 'prog-mode-hook 'outline-minor-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;;;; company

(use-package company
  :demand t
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("<tab>" . company-complete-selection))
  :config (global-company-mode))

;;;; crux

(use-package crux
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c f" . crux-recentf-find-file)))

;;;; Searching (ctrlf)

(use-package ctrlf
  :config (ctrlf-mode +1))

;; Always switch focus to the Occur buffer when running occur
(add-hook 'occur-hook
          #'(lambda ()
              (switch-to-buffer-other-window "*Occur*")))

;;;; dap

(use-package dap-mode
  :after lsp-mode
  :hook ((lsp-mode . dap-mode)
         (dap-mode . dap-ui-mode)
         (dap-mode . dap-tooltip-mode)
         (python-mode . (lambda()
                          (require 'dap-python)
                          (setq dap-python-debugger 'debugpy)))
         (go-mode . (lambda() (require 'dap-go)))))

;;;; diff-hl

(use-package diff-hl
  :demand t
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config (global-diff-hl-mode))

;;;; dired

(define-key global-map (kbd "C-c d") 'dired-jump-other-window)

;; Format for listing files
(setq dired-listing-switches "-aoh")

;; If there is a Dired buffer displayed in some window, use its current
;; directory, instead of this Dired buffer's current directory.
(setq dired-dwim-target t)

;; Reuse current buffer when pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; Always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;;;; docker

(use-package dockerfile-mode)

;;;; eshell

(setq eshell-aliases-file (expand-file-name "~/emacs/eshell/alias")
      ;; Do TAB completion
      eshell-cmpl-cycle-completions nil)

;;;; git, magit, forge

(use-package gitconfig-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.gitconfig\\'" . gitconfig-mode)))

(use-package magit
  :bind (:map magit-diff-mode-map
              ("M-<return>" . magit-diff-visit-file-other-window))
  :config
  (setq magit-diff-refine-hunk 'all))

;; Credentials are stored in ~/.authinfo
(use-package forge
  :after magit
  :hook (after-save . magit-after-save-refresh-status)
  :config
  (add-to-list 'forge-alist '("github-home"
                              "api.github.com"
                              "github.com"
                              forge-github-repository))
  (add-to-list 'forge-alist '("github-work"
                              "api.github.com"
                              "github.com"
                              forge-github-repository)))

;;;; github-browse-file

(use-package github-browse-file
  :bind ("C-h g" . github-browse-file))

;;;; gnuplot

(use-package gnuplot)

;;;; graphviz

(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

;;;; grip

;; Use grip-mode to view markdown (GFM) files rendered as HTML using GitHub's
;; rendering API.
;;
;; HEY CHRIS: Using work account requires setting CA cert.

(use-package grip-mode
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode))
  :config
  (require 'auth-source)
  (let ((credential (auth-source-user-and-password
                     "api.github.com" "chrisc-ironnet^forge")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential))))

;;;; key-chord

(use-package key-chord
  :config
  (key-chord-define-global "\\e" 'lsp-format-buffer)
  (key-chord-define-global "\\u" 'undo-tree-visualize)
  (key-chord-define-global "\\i" 'org-toggle-inline-images)
  (key-chord-define-global "\\p" 'python-pytest-dispatch)
  (key-chord-mode +1))

;;;; Minibuffer completion (selectrum, prescient, marginalia, consult)

(use-package selectrum
  :config (selectrum-mode +1))

(use-package selectrum-prescient
  :config (selectrum-prescient-mode +1))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult)

;;;; fish

(use-package fish-mode)

;;;; flycheck

(use-package flycheck
  :config (global-flycheck-mode +1))

;; Run more flycheck checkers in LSP mode. Unfortunately, LSP-mode disables all
;; flycheck checkers other than what's built in to the LSP server. From:
;; https://github.com/flycheck/flycheck/issues/1762

(defvar-local my/flycheck-local-cache nil)

(defun my/flycheck-checker-get (fn checker property)
  "Advice around the flycheck-checker-get function."
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall fn checker property)))

(advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)

(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'sh-mode)
              (setq my/flycheck-local-cache '((lsp . ((next-checkers . (sh-posix-bash)))))))
            (when (derived-mode-p 'go-mode)
              (setq my/flycheck-local-cache '((lsp . ((next-checkers . (go-golint)))))))))

;;;; helpful

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h C" . helpful-command)))

;;;; Images

;; Allow inline EPS images in org files

;; Commenting this out, because
;; - somehow this causes JSON files to open in Image mode
;; - requiring a fixed image size defined here is a non-starter

;; (setq imagemagick-enabled-types t)
;; (imagemagick-register-types)
;; (add-to-list 'image-file-name-extensions "eps")
;; (setq org-image-actual-width '(500))

;;;; LSP

(use-package lsp-mode
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui
  :config
  (setq lsp-ui-doc-position 'bottom
        lsp-ui-sideline-show-hover t))

;;;; markdown

(defun cfclrk/markdown-mode-hook ()
  "Customize `markdown-mode'."
  (turn-on-auto-fill)
  (display-fill-column-indicator-mode 1))

(use-package markdown-mode
  :hook (markdown-mode . cfclrk/markdown-mode-hook)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq whitespace-style '(face tabs empty trailing))
  (whitespace-mode -1)
  (whitespace-mode +1))

;;;; org

(load-file (expand-file-name "cfclrk_org.el" user-emacs-directory))

;;;; page-break-lines

(use-package page-break-lines
  :config (global-page-break-lines-mode))

;;;; projectile

(use-package projectile
  :demand t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode t)
  (add-to-list 'projectile-globally-ignored-directories "*.mypy_cache")
  (add-to-list 'projectile-globally-ignored-directories "*.pytest_cache")
  (add-to-list 'projectile-globally-ignored-directories "*logs")
  (add-to-list 'projectile-globally-ignored-directories "*_output")
  (add-to-list 'projectile-globally-ignored-directories "*cdk\.out")

  ;; TODO: is this necessary? Shouldn't projectile exclude items in the .gitignore?
  (add-to-list 'projectile-globally-ignored-directories "*.terraform")

  ;; Update projectile project detection to identify projects by the files in
  ;; `projectile-project-root-files'. Originally, projectile identifies projects
  ;; using a different set of files markers. Original value was:
  ;;
  ;;    (".projectile" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs")
  ;;
  ;; Projectile discovers projects by iterating through
  ;; projectile-project-root-files-bottom-up and checking each parent dir for
  ;; that file. It searches the entire dir lineage for the first item, then the
  ;; next. I don't think this is correct; it should iterate through the whole
  ;; list at each dir level.

  (setq projectile-project-root-files-bottom-up
		(-union projectile-project-root-files-bottom-up
				projectile-project-root-files))

  (defun projectile-root-bottom-up (dir &optional list)
  "Redefine this projectile function to discover most specific project.

Identify a project root in DIR by bottom-up search for files in LIST.
If LIST is nil, use `projectile-project-root-files-bottom-up' instead.
Return the first (bottommost) matched directory or nil if not found."
  (let ((marker-files (or list projectile-project-root-files-bottom-up)))
    (f--traverse-upwards
     (any-file-in-dir? marker-files it)
     dir)))

  ;; My kind of Python project, with a Makefile
  (projectile-register-project-type
   'python-cfclrk '("setup.py" "Makefile")
   :project-file "setup.py"
   :src-dir "src"
   :compile "make dev"
   :test "make test"
   :test-dir "tests"
   :test-prefix "test"
   :test-suffix"_test"))

;;;; protobuf

(use-package protobuf-mode)

;;;; rainbow-delimiters

(use-package rainbow-delimiters)

;;;; recentf

;; Saves recent file names in $user-emacs-directory/recentf. View recent files
;; with C-c f (I configure that keybinding in the crux section).

(setq recentf-max-saved-items 300
	  recentf-max-menu-items 15)
(recentf-mode +1)

;;;; reveal-in-osx-finder

(use-package reveal-in-osx-finder)

;;;; setenv-file

(use-package setenv-file
  :straight (setenv-file :type git :host github :repo "cfclrk/setenv-file")
  :config
  (setq setenv-file-dir (expand-file-name "~/.env/")))

;;;; smartparens

(use-package smartparens
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode t)

  ;; slurping and barfing
  (define-key smartparens-mode-map (kbd "S-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "S-<left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-S-<right>") 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-S-<left>") 'sp-backward-slurp-sexp)

  ;; Create a key prefix
  (define-prefix-command 'sp-prefix-key-map)
  (define-key smartparens-mode-map (kbd "M-p") sp-prefix-key-map)

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

;;;; super-save

(use-package super-save
  :config
  (super-save-mode +1)
  (add-to-list 'super-save-triggers 'ace-window))

;;;; toml-mode

(use-package toml-mode)
(add-to-list 'auto-mode-alist '("Pipfile\\'" . toml-mode))

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

;;;; which-key

(use-package which-key
  :config
  (which-key-mode +1)
  (setq which-key-idle-delay 0.5
		which-key-idle-secondary-delay 0.1))

;;;; winner-mode

(setq winner-dont-bind-my-keys t)
(winner-mode)

;; Use "C-c q" to close a Help buffer, Compilation buffer, etc I just opened.
(define-key winner-mode-map (kbd "C-c q") #'winner-undo)

;;;; yaml

(use-package yaml-mode)

;;;; yasnippet

;; LSP uses yasnippet to expand snippets. So enabling yas-global-mode is
;; necessary even if you don't load any snippets.

(use-package yasnippet
  :config (yas-global-mode 1))

;;; Programming Languages
;;  ----------------------------------------------------------------------------

;;;; General (prog-mode)

(add-hook 'prog-mode-hook 'cfclrk/text-editing-hook)

;;;; Clojure

(use-package clojure-mode)

(use-package cider
  :after clojure-mode)

;;;; CSS and SCSS

(defun cfclrk/css-mode-hook ()
  "Customize `css-mode' and derived modes like `scss-mode'."
  (setq indent-tabs-mode nil
        css-indent-offset 2))

(add-hook 'css-mode-hook #'cfclrk/css-mode-hook)
;; (add-hook 'css-mode-hook #'lsp-deferred)

;;;; Golang

(defun cfclrk/go-mode-hook ()
  "Hooks to add in `go-mode' buffers."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :hook ((go-mode . lsp-deferred)
         (go-mode . cfclrk/go-mode-hook))
  :config
  (setq godoc-at-point-function 'godoc-gogetdoc
        gofmt-command (executable-find "goimports")
        fill-column 100
        whitespace-style '(face tabs empty trailing))

  ;; Restart whitespace mode to correctly use fill-column
  (whitespace-mode -1)
  (whitespace-mode +1))

;; An Emacs mode for running "go test" in a compilation buffer.
;; https://github.com/nlamirault/gotest.el
(use-package gotest
  :after go-mode
  :bind-keymap ("C-t" . go-test-mode-map)
  :bind (:map go-test-mode-map
         ("p" . go-test-current-project)
         ("f" . go-test-current-file)
         ("c" . go-test-current-coverage)
         ("t" . go-test-current-test)
         :map go-mode-map
         ("<f5>" . go-test-current-test)
         ("<f6>" . go-test-current-file))
  :config
  (setq go-test-args "-v"))

;; Provides the ability to add struct tags using gomodifytags. Requires
;; gomodifytags, which can be install with:
;;
;;     go get github.com/fatih/gomodifytags
(use-package go-tag
  :after go-mode)

;; Provides ability to fill in all key values in a struct. Requires fillstruct,
;; which can be installed with:
;;
;;    go get -u github.com/davidrjenni/reftools/cmd/fillstruct
;;
;; Struct has to have opening and closing {} braces, and cursor at beginning of
;; struct name I believe.
(use-package go-fill-struct
  :after go-mode)

;;;; Groovy

(use-package groovy-mode)

;;;; Javascript (and JSON)

(defun cfclrk/js-mode-hook ()
  "Customize `js-mode'."
  (setq js-indent-level 2))
(add-hook 'js-mode-hook #'cfclrk/js-mode-hook)

;;;; Lisp

(defun cfclrk/lisp-mode-hook ()
  "General configuration for any Lisp."
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)

  ;; Restart whitespace mode so that it properly uses fill-column.
  (setq fill-column 80)
  (whitespace-mode -1)
  (whitespace-mode +1))

(dolist (hook '(lisp-mode-hook
                clojure-mode-hook
                emacs-lisp-mode-hook
                lisp-data-mode-hook))
  (add-hook hook #'cfclrk/lisp-mode-hook))

;;;; Python

(defun cfclrk/python-mode-hook ()
  "Customize `python-mode'."
  (setq fill-column 88
        python-fill-docstring-style 'pep-257-nn
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i")

  ;; Restart whitespace-mode so that it properly uses `fill-column'
  (whitespace-mode -1)
  (whitespace-mode +1))

(add-hook 'python-mode-hook #'cfclrk/python-mode-hook)

(use-package python-pytest)

(use-package pyenv-mode)

;; LSP using the pyright language server
(use-package lsp-pyright
  :init (setq lsp-pyright-multi-root nil)
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;;;; Rust

;; I am using the rust-analyzer LSP server.

(use-package rustic)

;;;; Terraform

(use-package terraform-mode
  :hook (terraform-mode . lsp-deferred)
  :init (setq lsp-terraform-server '("terraform-ls" "serve")))

;;; init.el ends here
