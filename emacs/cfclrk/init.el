;;; init.el -- My emacs setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Startup
;;  ----------------------------------------------------------------------------

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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

(setq
 ;; Make every use-package declaration use straight. This also accomplishes what
 ;; `use-package-always-ensure' does.
 straight-use-package-by-default t
 ;; Make straight use ssh instead of https
 straight-vc-git-default-protocol 'ssh)

;; Log a message about startup time
(defun my/startup-hook ()
  "Show me some Emacs startup stats."
  (interactive)
  (message "*** Emacs loaded in %s with %d garbage collections."
           (format "%.1f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook 'my/startup-hook)

;;; Early init - Org Mode and dash
;;  ----------------------------------------------------------------------------

;; Prevent loading the built-in org-mode. Instead, use straight to get org-mode.
;; We must run this before anything else loads the built-in org-mode. Straight
;; gets org-mode from the mirror here: https://github.com/emacs-straight
(use-package org)

;; At the top because I use dash in my own functions.
(use-package dash
  :config
  ;; Fontify dash-defined anaphoric vars ("it", "acc", etc)
  (global-dash-fontify-mode)

  ;; Enable C-h S (info-lookup-symbol) on dash symbols
  (with-eval-after-load 'info-look
    (dash-register-info-lookup)))

;;; Functions
;;  ----------------------------------------------------------------------------

(defun my/lsp-remove-all-workspaces ()
  "Clear all LSP workspaces. Sometimes this fixes things."
  (interactive)
  (mapc
   'lsp-workspace-folders-remove
   (lsp-session-folders (lsp-session))))

(defun upsert-alist (quoted-alist entry)
  "Insert or update ENTRY in QUOTED-ALIST.

First delete the entry if it is in QUOTED-ALIST, then insert the
new ENTRY.

Examples:

  (setq my-alist '((:foo . :fooo)))
  (upsert-alist 'my-alist '(:bar . :baar))
."
  (let ((entry-key (car entry))
        (orig-alist (symbol-value quoted-alist)))
    (set quoted-alist
         (cons entry (assoc-delete-all entry-key orig-alist)))))

(defun pprint (form &optional printcharfun)
  "Return a pretty-printed version of FORM.

Optional PRINTCHARFUN is as defined by `princ'."
  (princ (with-temp-buffer
           (cl-prettyprint form)
           (buffer-string))
         printcharfun))

(defun my/show-buffer-file-name ()
  "Display and copy the full path to the current file.
Adapted from Emacs Redux (emacsredux.com)."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (message filename)
    (kill-new filename)))

;; C-c z to see full path of file in the current buffer
(global-set-key (kbd "C-c z") 'my/show-buffer-file-name)

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

;; What was this before?
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)

;;; Theme, Font, Display
;;  ----------------------------------------------------------------------------

;; Use a larger font on big monitors
(when window-system
  (if (> (nth 2 (frame-monitor-attribute 'geometry)) 1600)
      (set-face-attribute 'default nil :height 140)))

;; Use the doom-one theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Make line-number color more visible. Original is 'dim gray.
(set-face-foreground 'line-number "gray")

(defun my/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

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

(setq display-buffer-alist
      '(("\\*eshell\\*" display-buffer-use-some-window)
        ("\\*Help\\*" display-buffer-same-window)))

;;; Emojis
;;  ----------------------------------------------------------------------------

;; To insert an emoji, use one of the following functions:
;;
;;  - all-the-icons-insert-*
;;    [octicon, faicon, fileicon, material, wicon, alltheicon]
;;  - emojify-insert-emoji
;;
;; How does this change in Emacs 29?

;; Some more options for inserting emojis.
(use-package emojify)

;;; Editor General
;;  ----------------------------------------------------------------------------

(blink-cursor-mode -1)       ; Just a nice, solid cursor
(global-auto-revert-mode t)  ; Revert buffers when their backing files change
(set-language-environment "UTF-8")

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default fill-column 80)

(setq column-number-mode t           ; Show line:column in mode line
      make-backup-files nil
      inhibit-splash-screen t        ; Do not show the welcome screen
      sentence-end-double-space nil
      help-window-select t
      delete-by-moving-to-trash t)

;; Change all yes/no prompts to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Show line numbers in any text or prog mode.
; (add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Make shell scripts executable when saved
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; A .zsh file is a shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; Make nicer keybinding for `xref-find-definitions-other-window'
(define-key global-map
            (kbd "C-c M-.")
            'xref-find-definitions-other-window)

;; Keep customizations outside of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Remap C-h g from `describe-gnu-project' to `github-browse-file'
(global-set-key (kbd "C-h g") 'github-browse-file)

;; MacOS
(when (eq system-type 'darwin)
  ;; Set the default font to Roboto Mono
  (set-face-attribute 'default nil
                      :family "Roboto Mono"
                      :weight 'normal)

  ;; ⌘ as Meta and ⎇ as Super
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'hyper)

  ;; Enable emoji
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;;; Text
;;  ----------------------------------------------------------------------------

;; whitespace
(require 'whitespace)
(setq whitespace-style '(face tabs empty trailing lines-tail)
      whitespace-line-column nil)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; text mode
(defun my/text-editing-hook ()
  "Minor modes that I want enabled in pretty much every textual buffer."
  (smartparens-mode +1)
  (whitespace-mode +1)
  (delete-selection-mode +1))

(add-hook 'text-mode-hook 'my/text-editing-hook)

;; Convert a string to titlecase
(use-package titlecase)

;;;; URLs

(require 'xwidget)

(add-to-list
 'browse-url-handlers
 '("https://docs.oracle.com/en/.*.html" . xwidget-webkit-browse-url))


;;;; Registers

(setq register-preview-delay 0)

;;; Packages/Modes
;;  ----------------------------------------------------------------------------

;;;; adoc-mode

;; For reading asciidoc
(use-package adoc-mode)

;;;; ace-window

;; Switch focus between visible windows.

(use-package ace-window
  :bind ("M-l" . ace-window)
  :config (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

;;;; bazel

(use-package bazel
  :straight (bazel
             :host github
             :repo "bazelbuild/emacs-bazel-mode")
  :custom
  (bazel-buildifier-before-save t))

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

;;;; compilation

;; Render ANSI color codes in all compilation buffers.

(require 'ansi-color)

(defun colorize-compilation-buffer ()
  "Apply `ansi-color-apply-on-region' on the whole compilation buffer.
From: https://stackoverflow.com/a/3072831/340613"
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

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

;;;; docker

(use-package dockerfile-mode)

;; Edit files in a docker container like:
;;
;;   C-x C-f /docker:root@mycontainer:/app/server.py
(use-package docker-tramp)

;;;; envars

(use-package envars
  :straight (envars
             :host github
             :repo "cfclrk/envars")
  :config
  (setq envars-dir (expand-file-name "~/.env/")))

;;;; eshell

(setq ;; Do TAB completion
 eshell-cmpl-cycle-completions nil)

;;;; git, magit, forge

;; git-modes (https://github.com/magit/git-modes) is developed under the
;; umbrella of magit, and provides "gitattributes-mode", "gitconfig-mode", and
;; "gitigrone-mode".

(use-package git-modes
  :config
  ;; gitconfig-mode
  (add-to-list 'auto-mode-alist '("\\.gitconfig\\'" . gitconfig-mode))
  ;; gitignore-mode
  (add-to-list 'auto-mode-alist '("/.dockerignore\\'" . gitignore-mode)))

(use-package magit
  :bind (:map magit-diff-mode-map
              ("<C-return>" . magit-diff-visit-file-other-window)
              ("<M-return>" . magit-diff-visit-worktree-file-other-window))
  :config
  (setq magit-diff-refine-hunk 'all))

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

;;;; gnuplot

(use-package gnuplot)

;;;; graphviz

(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

;;;; ispell

(setq ispell-program-name "aspell")

;;;; key-chord

(use-package key-chord
  :config
  (key-chord-define-global "\\a" 'treemacs-select-window)
  (key-chord-define-global "\\e" 'lsp-format-buffer)
  (key-chord-define-global "\\u" 'undo-tree-visualize)
  (key-chord-define-global "\\i" 'org-toggle-inline-images)
  (key-chord-define-global "\\p" 'python-pytest-dispatch)
  (key-chord-mode +1))

;;;; Minibuffer completion (vertico, orderless, marginalia, consult)

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :init (marginalia-mode))

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(substring orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult)

;;;; fish

(use-package fish-mode)

;;;; flycheck

(use-package flycheck
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode +1))

;; Run more flycheck checkers in LSP mode. LSP-mode disables all flycheck
;; checkers (because such checks are instead delegated to the LSP server). From:
;; https://github.com/flycheck/flycheck/issues/1762

(defvar-local my/flycheck-local-cache nil)

(defun my/flycheck-checker-get (fn checker property)
  "Advice around the flycheck-checker-get function.
FN, CHECKER, PROPERTY as documented in flycheck-checker-get."
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall fn checker property)))

(advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)

(add-hook
 'lsp-managed-mode-hook
 (lambda ()
   (when (derived-mode-p 'sh-mode)
     (setq my/flycheck-local-cache
           '((lsp . ((next-checkers . (sh-posix-bash)))))))
   (when (derived-mode-p 'go-mode)
     (setq my/flycheck-local-cache
           '((lsp . ((next-checkers . (go-golint)))))))))

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

;;;; Ligatures

;; Taken from the hasklig README.md here: https://github.com/i-tu/Hasklig
;; (setq hasklig-ligatures
;;       '("<*" "<*>" "<+>" "<$>" "***" "<|" "|>" "<|>" "!!"
;;         "||" "===" "==>" "<<<" ">>>" "<>" "+++" "<-" "->"
;;         "=>" ">>" "<<" ">>=" "=<<" ".." "..." "::" "-<"
;;         ">-" "-<<" ">>-" "++" "/=" "=="))

;; (use-package ligature
;;   :straight (ligature
;;              :host github
;;              :repo "mickeynp/ligature.el")
;;   :config
;;   (ligature-set-ligatures
;;    'clojure-mode
;;    '("->" "->>" "<>" "=>"))
;;   (global-ligature-mode t))

;;;; line

;; Highlight the current line in certain modes. The modes in which this is
;; active is defined by `lin-mode-hooks'.
(use-package lin
  :config
  (customize-set-variable 'lin-face 'consult-preview-line)
  (lin-global-mode 1))

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
  (lsp-ui-peek-show-directory nil)
  (lsp-ui-doc-max-height 20))


;;;; magic-mode-alist

;; Use restclient mode for buffers that start with HTTP verbs. This could be a
;; cool idea for `http' blocks in markdown files. But, I'd want the buffers to
;; also be able to include some generated authentication stuff.

;; (upsert-alist
;;  'magic-mode-alist
;;  '("^GET " . restclient-mode))

;;;; markdown

(load
 (expand-file-name "init-markdown.el" user-emacs-directory))

;;;; mermaid

(use-package mermaid-mode
  :config
  (setq mermaid-output-format ".svg"))

;;;; org

(load
 (expand-file-name "init-org.el" user-emacs-directory))

;;;; page-break-lines

(use-package page-break-lines
  :config
  (global-page-break-lines-mode)

  ;; On MacOS, the lines were a little too long and wrapped a bit. This snippet
  ;; is verbatim from the [README][1]. Apparently, a different font was being
  ;; used for the symbol used to create the horizontal rule.
  ;;
  ;; [1]: https://github.com/purcell/page-break-lines#issues-and-limitations
  (set-fontset-font
   "fontset-default"
   (cons page-break-lines-char page-break-lines-char)
   (face-attribute 'default :family)))

;;;; projectile

(use-package projectile
  :demand t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (load (expand-file-name "~/emacs/projectile-discovery.el"))
  (projectile-mode +1)
  (setq projectile-use-git-grep t))

;;;; rainbow-delimiters

(use-package rainbow-delimiters)

;;;; recentf

;; Saves recent file names in $user-emacs-directory/recentf. View recent files
;; with C-c f (I configure that keybinding in the crux section).

(setq recentf-max-saved-items 300
	  recentf-max-menu-items 15)
(recentf-mode +1)

;;;; restclient

(use-package restclient)

;;;; reveal-in-osx-finder

(use-package reveal-in-osx-finder)

;;;; savehist

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode)
  :config
  (setq history-length 25))

;;;; smartparens

(use-package smartparens
  :bind (:map lisp-mode-map
              ("M-f" . sp-next-sexp)
              ("M-b" . sp-backward-sexp))
  :init
  (require 'smartparens-config)
  :config
  (load (expand-file-name "~/emacs/smartparens.el"))
  (my-smartparens-config))

;;;; super-save

(use-package super-save
  :config
  (super-save-mode +1)
  (add-to-list 'super-save-triggers 'ace-window))

;;;; treemacs

(use-package treemacs
  :config
  (treemacs-resize-icons 18))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

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

;;;; visual-fill-column

(use-package visual-fill-column)

;; To automatically use visual-fill-column whenever you enter visual-line-mode:
;; (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)

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

;;;; web-mode

(use-package web-mode
  :config
  (setq web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-sql-indent-offset 2))

;;;; yaml

(use-package yaml-mode)

;;;; yasnippet

;; LSP uses yasnippet to expand snippets. So enabling yas-global-mode is
;; necessary even if you don't load any snippets.

(use-package yasnippet
  :config (yas-global-mode 1))

;;; Programming Languages
;;  ----------------------------------------------------------------------------

;;;; General

(add-hook 'prog-mode-hook 'my/text-editing-hook)

(defun my/lisp-mode-hook ()
  "General configuration for any LISP."
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (setq fill-column 80)
  ;; Restart whitespace mode so that it properly uses `fill-column'.
  (whitespace-mode -1)
  (whitespace-mode +1))

;;;; shell/bash/zsh

(add-hook 'sh-mode-hook #'lsp-deferred)

;;;; C and C++

;; (add-hook c-mode-hook #lsp-deferred)
;; (add-hook c++-mode-hook #lsp-deferred)

;;;; Clojure

;; From:
;; https://ag91.github.io/blog/2022/06/09/make-adding-a-clojure-require-more-interactive-with-cider-and-cljr/
(defun my/make-cljr-add-use-snippet-interactive ()
  (setq-local
   cljr--add-use-snippet
   "[${1:$$(yas-choose-value
             (ignore-errors
               (cider-sync-request:ns-list)))} :refer ${2:[$3]}]"))

;; - TODO: Figure out how to make `xref-find-definitions' work when a file is
;;         not loaded in cider
(use-package cider
  :straight (cider
             :host github
             :repo "clojure-emacs/cider"
             :fork (:host github
                    :repo "cfclrk/cider"
                    :branch "bazel-support"))
  :after clojure-mode
  ;; TODO: :bind cider-pprint-eval-last-sexp-to-comment to C-j
  :bind (:map cider-mode-map
              ("C-t n" . cider-test-run-ns-tests)
              ("C-t p" . cider-test-run-project-tests)
              ("C-t t" . cider-test-run-test))
  :hook ((cider-repl-mode . (lambda () (smartparens-mode +1)))
         (cider-repl-mode . (lambda () (rainbow-delimiters-mode +1)))
         (cider-mode . my/make-cljr-add-use-snippet-interactive))
  :custom
  ;; Automatically save files before they are loaded in the repl
  (cider-save-file-on-load t)
  ;; Add a newline to the repl prompt
  (cider-repl-prompt-function (lambda (namespace)
                                (format "%s\n> " namespace))))


(use-package clojure-mode
  :mode "\\.cljstyle\\'"  ; Use clojure-mode for ".cljstyle" files
  :hook ((clojure-mode . lsp-deferred)
         (clojure-mode . my/lisp-mode-hook)
         (clojure-mode . cljstyle-format-on-save-mode))
  :custom
  ;; Indent arguments instead of aligning them
  (clojure-indent-style 'always-indent)
  :config
  (setq clojure-build-tool-files (add-to-list
                                  'clojure-build-tool-files
                                  "WORKSPACE"))
  (setq clojure-build-tool-files (add-to-list
                                  'clojure-build-tool-files
                                  "WORKSPACE.bazel")))

(use-package cljstyle-format
  :after clojure-mode)

(use-package zprint-format)

;; Use C-c C-r
(use-package clj-refactor
  :hook ((clojure-mode . (lambda () (clj-refactor-mode 1))))
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package stonehenge
  :after cider
  :straight (stonehenge
             :local-repo "~/Work/stonehenge"
             :files ("development/emacs/stonehenge.el"))
  :config
  (customize-set-variable 'stonehenge-dir
                          (expand-file-name "~/Work/stonehenge")))

;; (use-package monorepl
;;   :after cider
;;   :straight (monorepl
;;              :local-repo "~/Work/stonehenge"
;;              :files ("development/emacs/monorepl.el"))
;;   :config
;;   (setq monorepl-STONEHENGE-PATH
;;         (expand-file-name "~/Work/stonehenge")))

;;;; CSS and SCSS

(defun my/css-mode-hook ()
  "Customize `css-mode' and derived modes like `scss-mode'."
  (setq indent-tabs-mode nil
        css-indent-offset 2))

(add-hook 'css-mode-hook #'my/css-mode-hook)
(add-hook 'css-mode-hook #'lsp-deferred)

;;;; Emacs Lisp

(defun my/emacs-lisp-mode-hook ()
  "Customize `emacs-lisp-mode'."
  ;; Fix the ridiculous default indentation for plists. See:
  ;; https://stackoverflow.com/q/22166895/340613
  (setq lisp-indent-function 'common-lisp-indent-function))

(add-hook 'emacs-lisp-mode-hook #'my/lisp-mode-hook)
;(add-hook 'emacs-lisp-mode-hook #'my/emacs-lisp-mode-hook)

;;;; Golang

(defun my/go-mode-hook ()
  "Hooks to add in `go-mode' buffers."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :hook ((go-mode . lsp-deferred)
         (go-mode . my/go-mode-hook))
  :config
  (setq godoc-at-point-function 'godoc-gogetdoc
        gofmt-command (executable-find "gci")
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
              ("f" . go-test-current-file)
              ("c" . go-test-current-coverage)
              ("t" . go-test-current-test))
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
;;
;; TODO: I think LSP might provide this now.
(use-package go-fill-struct
  :after go-mode)

;;;; Groovy

(use-package groovy-mode
  :hook (groovy-mode . lsp-deferred)
  :config
  (setq groovy-indent-offset 2))

;;;; Haskell

(use-package haskell-mode
  :hook ((haskell-mode . lsp-deferred)
         (haskell-literate-mode . lsp-deferred)))

(use-package lsp-haskell
  :after lsp-mode)

;; Use font ligatures
(use-package hasklig-mode
  :hook ((haskell-mode)
         (inferior-haskell-mode)))

;; projectile -- discover projects with a ".cabal" file
(add-to-list
 'my-project-root-files
 (lambda (dir)
   "Non-nil if a file with a .cabal extension is in dir"
   (member "cabal" (-map 'f-ext (f-entries dir)))))

;;;; Java

;; lsp-java uses the Eclipse JDT Language Server. See:
;; https://github.com/eclipse/eclipse.jdt.ls

(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :config
  (setq lsp-java-format-settings-url
        (concat "https://raw.githubusercontent.com/"
                "google/styleguide/gh-pages/eclipse-java-google-style.xml"))
  (setq lsp-java-format-settings-profile "GoogleStyle"
        c-basic-offset 2))

(add-hook 'java-mode-hook #'lsp-deferred)

;;;; Javascript and JSON

(defun my/js-mode-hook ()
  "Customize `js-mode'."
  (setq js-indent-level 2))

(add-hook 'js-mode-hook #'my/js-mode-hook)
(add-hook 'js-mode-hook #'lsp-deferred)

(use-package js2-mode)

;;;; Lisp

;; TODO: figure out how to make fill-paragraph respect markdown formatting. E.g.
;; reformatting a bulleted list puts everyting on the same line right now.

(dolist (hook '(lisp-mode-hook
                lisp-data-mode-hook))
  (add-hook hook #'my/lisp-mode-hook))

;;;; PHP

(use-package php-mode)

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

(add-hook 'python-mode-hook #'my/python-mode-hook)

(use-package python-pytest
  ;; To run as "pytest -s", save "-s" opt to `transient-values-file'
  :after python
  :bind (:map python-mode-map
              ("C-t t" . python-pytest-function)
              ("C-t f" . python-pytest-file)
              ("C-t l" . python-pytest-last-failed)))

(use-package pyenv-mode)

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

;; Add a new kind of Projectile project for python projects that are structured
;; like py-demo.
(projectile-register-project-type
 'python-cfclrk '("setup.py" "Makefile")
 :project-file "setup.py"
 :src-dir "src"
 :compile "make dev"
 :install "make dev"
 :test "make test"
 :test-dir "tests"
 :test-prefix "test"
 :test-suffix"_test")

;;;; Rust

;; rustic starts a rust-analyzer LSP server.
(use-package rustic
  :bind (:map rustic-mode-map
              ("C-t t" . rustic-cargo-current-test))
  :config
  (setq
   ;; rustic-format-on-save was causing some problems with LSP.
   ;; rustic-format-on-save t
   rustic-test-arguments "-- --show-output"))

;;;; SQL

;; (setq sql-postgres-login-params nil)

(use-package sqlformat
  :config
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g")))

;;;; Terraform

(use-package terraform-mode
  :hook (terraform-mode . lsp-deferred)
  :init (setq lsp-terraform-server '("terraform-ls" "serve")))

;;;; WebAssembly

(use-package wat-mode
  :straight (wat-mode
             :host github
             :repo "devonsparks/wat-mode"))

;;;; YAML

(add-hook 'yaml-mode-hook #'lsp-deferred)

;; Better YAML LSP support for CloudFormation
(setq lsp-yaml-custom-tags
      ["!Base64"
       "!Cidr"
       "!FindInMap sequence"
       "!GetAtt"
       "!GetAZs"
       "!ImportValue"
       "!Join sequence"
       "!Select sequence"
       "!Split sequence"
       "!Sub"
       "!Ref"
       "!And"
       "!Equals"
       "!If"
       "!Not"
       "!Or"])

;;; Garbage collect

(garbage-collect)

;; Restore original GC values
(add-hook 'emacs-startup-hook
		  (lambda ()
			(setq gc-cons-threshold gc-cons-threshold-original)
			(setq gc-cons-percentage gc-cons-percentage-original)))

;;; init.el ends here
