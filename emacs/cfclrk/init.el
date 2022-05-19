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

(setq
 ;; Make every use-package declaration use straight. This also accomplishes what
 ;; `use-package-always-ensure' does.
 straight-use-package-by-default t
 ;; Make straight use ssh instead of https
 straight-vc-git-default-protocol 'ssh)

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

;;; Early init - Org Mode and dash
;;  ----------------------------------------------------------------------------

;; Prevent loading the built-in org-mode. Instead, use straight to get org-mode.
;; We must run this before anything else loads the built-in org-mode. These come
;; from the straight mirror here: https://github.com/emacs-straight
(use-package org)
(use-package org-contrib)

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

(defun cfc/lsp-remove-all-workspaces ()
  "Clear all LSP workspaces. Sometimes this fixes things."
  (interactive)
  (mapc
   'lsp-workspace-folders-remove
   (lsp-session-folders (lsp-session))))

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

;; What was this before?
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)

;;; Theme, Font, Display
;;  ----------------------------------------------------------------------------

;; Use Source Code Pro on MacOS
(when (eq system-type 'darwin)
  ;; (set-face-attribute 'default nil :family "Source Code Pro")

  ;; Requires installing the Hasklig font. See:
  ;; https://github.com/i-tu/Hasklig
  (set-face-attribute 'default nil
                      :family "Hasklig"
                      :weight 'normal))

;; Use a larger font on big monitors
(when window-system
  (if (> (nth 2 (frame-monitor-attribute 'geometry)) 1600)
      (set-face-attribute 'default nil :height 160)))

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
 (setq doom-modeline-buffer-encoding nil
       doom-modeline-height 40
       doom-modeline-hud t
       doom-modeline-project-detection 'projectile
       doom-modeline-buffer-file-name-style 'buffer-name
       doom-modeline-vcs-max-length 15
       doom-modeline-env-version nil))

(setq display-buffer-alist
      '(("\\*eshell\\*" display-buffer-use-some-window)
        ("\\*Help\\*" display-buffer-same-window)))

;;; Editor General
;;  ----------------------------------------------------------------------------

(tool-bar-mode -1)           ; No tool bar, which has the save button, etc
(scroll-bar-mode -1)         ; No scroll bars to the right of buffers
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

;; Show line numbers in any text mode.
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; whitespace
(require 'whitespace)
(setq whitespace-style '(face tabs empty trailing lines-tail)
      whitespace-line-column nil)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; text mode
(defun cfclrk/text-editing-hook ()
  "Minor modes that I want enabled in pretty much every textual buffer."
  (smartparens-mode +1)
  (whitespace-mode +1)
  (delete-selection-mode +1))

(add-hook 'text-mode-hook 'cfclrk/text-editing-hook)

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

;;;; URLs

(require 'xwidget)

(add-to-list
 'browse-url-handlers
 '("https://docs.oracle.com/en/.*.html" . xwidget-webkit-browse-url))


;;;; Registers

(setq register-preview-delay 0)

;;; Packages/Modes
;;  ----------------------------------------------------------------------------

;;;; ace-window

;; Switch focus between visible windows.

(use-package ace-window
  :bind ("M-l" . ace-window)
  :config (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

;;;; bazel

(use-package bazel
  :straight (bazel :type git :host github :repo "bazelbuild/emacs-bazel-mode"))

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

;; Edit files in a docker container like:
;;
;;   C-x C-f /docker:root@mycontainer:/app/server.py
(use-package docker-tramp)

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

(use-package magit-todos)

;; Credentials are stored in ~/.authinfo
(use-package forge
  :after magit
  :hook (after-save . magit-after-save-refresh-status)
  :config
  ;; (add-to-list 'forge-alist '("github-home"
  ;;                             "api.github.com"
  ;;                             "github.com"
  ;;                             forge-github-repository))
  ;; (add-to-list 'forge-alist '("github-work"
  ;;                             "api.github.com"
  ;;                             "github.com"
  ;;                             forge-github-repository))
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

;;;; grip

;; Use grip-mode to view markdown (GFM) files rendered as HTML using GitHub's
;; rendering API.

(use-package grip-mode
  :after markdown-mode
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode))
  :config
  (require 'auth-source)
  (let ((credential (auth-source-user-and-password
                     "api.github.com" "cclark-splash^forge")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential))))

;;;; ispell

(setq ispell-program-name "aspell")

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
  :config
  (global-flycheck-mode +1))

;; Run more flycheck checkers in LSP mode. LSP-mode disables all flycheck
;; checkers (instead that is delegated to the LSP server). From:
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

;; TODO: enable again once https://github.com/Wilfred/elisp-refs/issues/35 is
;; resolved.
;; (use-package helpful
;;   :bind (("C-h f" . helpful-callable)
;;          ("C-h v" . helpful-variable)
;;          ("C-h k" . helpful-key)
;;          ("C-c C-d" . helpful-at-point)
;;          ("C-h C" . helpful-command)))

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

;;;; markdown

(defun markdown-live-preview-window-xwidget-webkit (file)
  "Preview FILE with xwidget-webkit.
To be used with `markdown-live-preview-window-function'."
  (let ((uri (format "file://%s" file)))
      (xwidget-webkit-browse-url uri)
      xwidget-webkit-last-session-buffer))

(defun my-markdown-mode-hook ()
  "Customize `markdown-mode'."
  ;; automatically insert a newline at fill-column
  ;; (turn-on-auto-fill)

  ;; Show a vertical line where fill-column is
  ;; (display-fill-column-indicator-mode 1)
  )

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . visual-fill-column-mode)
         (markdown-mode . my-markdown-mode-hook))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  ;; TODO: Update/monkeypatch markdown-add-xhtml-header-and-footer. Use mustache
  ;; templates.
  (setq markdown-command "multimarkdown"
        markdown-fontify-code-blocks-natively t
        markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-make-gfm-checkboxes-buttons t
        markdown-content-type "application/xhtml+xml"
        markdown-split-window-direction 'right
        markdown-content-type "")
  (setq markdown-live-preview-window-function
        #'markdown-live-preview-window-xwidget-webkit)

  ;; A file I made using generate-github-markdown-css
  (setq my-markdown-css (expand-file-name
                         "github_dark_dimmed.css"
                         user-emacs-directory))

  (setq
   markdown-content-type "application/xhtml+xml"

   ;; Note, these styles are only applied to the div with a "markdown-body"
   ;; class attribute. markdown-mode does not automatically add a
   ;; `class="markdown-body"' anywhere, so we need to do that ourselves
   ;; somewhere (I do it in the header.html file below).
   ;;
   ;; Also, the URL below is some GitHub CSS, available on a CDN. But, it only
   ;; includes styles for the "dark" and "light" GitHub themes. The xwdidget
   ;; browser wil use one of those - whichever your system theme is. Using the
   ;; dark-dimmed theme requires some other solution. Also, I like having local
   ;; CSS so it doesn't have to be fetched over the internet.
   ;;
   ;; - "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.1.0/github-markdown.min.css"
   markdown-css-paths
   `(;; A file I made using generate-github-markdown-css
     ,my-markdown-css

     ;; For highlighting code blocks with highlightjs
     "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.1/styles/github-dark-dimmed.min.css")

   markdown-xhtml-header-content
   (f-read-text
    (file-truename
     (expand-file-name
      "markdown/header.html"
      user-emacs-directory))))
  :config
  (setq whitespace-style '(face tabs empty trailing))

  ;; Restart whitespace mode so that is properly uses `whitespace-style'.
  (whitespace-mode -1)
  (whitespace-mode +1))

;; edit-inderect is required to use C-c ' (markdown-edit-code-block), which lets
;; you edit source blocks in another buffer (similar to org-edit-special)
(use-package edit-indirect)

;;;; mermaid

(use-package mermaid-mode)

;;;; org

(load-file (expand-file-name "cfclrk_org.el" user-emacs-directory))

;;;; page-break-lines

(use-package page-break-lines
  :config (global-page-break-lines-mode))

;;;; projectile

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode t)

  ;; Never cache project root
  (setq projectile-enable-caching nil)

  ;; TODO: Are these necessary? Shouldn't projectile exclude items in the .gitignore?
  (add-to-list 'projectile-globally-ignored-directories "*logs")
  (add-to-list 'projectile-globally-ignored-directories "*_output")
  (add-to-list 'projectile-globally-ignored-directories "*.mypy_cache")
  (add-to-list 'projectile-globally-ignored-directories "*.pytest_cache")
  (add-to-list 'projectile-globally-ignored-directories "*cdk\.out")
  (add-to-list 'projectile-globally-ignored-directories "*.terraform")

  (load (expand-file-name "~/emacs/projectile-discovery.el")))

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

(add-hook 'prog-mode-hook 'cfclrk/text-editing-hook)

(defun my-lisp-mode-hook ()
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

(use-package clojure-mode
  :mode "\\.cljstyle\\'"  ; Use clojure-mode for ".cljstyle" files
  :hook ((clojure-mode . lsp-deferred)
         (clojure-mode . my-lisp-mode-hook)
         (clojure-mode . cljstyle-format-on-save-mode))
  :bind (:map clojure-mode-map
              ("C-t n" . cider-test-run-ns-tests)
              ("C-t p" . cider-test-run-project-tests)
              ("C-t t" . cider-test-run-test)))

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

(use-package stonehenge
  :after cider
  :straight (stonehenge
             :local-repo "~/Work/stonehenge"
             :files ("development/emacs/stonehenge.el"))
  :config
  (setq stonehenge-path (expand-file-name "~/Work/stonehenge")))

;; (use-package monorepl
;;   :after cider
;;   :straight (monorepl
;;              :local-repo "~/Work/stonehenge"
;;              :files ("development/emacs/monorepl.el"))
;;   :config
;;   (setq stonehenge-path "/Users/cclark/Work/stonehenge"))

;;;; CSS and SCSS

(defun cfclrk/css-mode-hook ()
  "Customize `css-mode' and derived modes like `scss-mode'."
  (setq indent-tabs-mode nil
        css-indent-offset 2))

(add-hook 'css-mode-hook #'cfclrk/css-mode-hook)
;; (add-hook 'css-mode-hook #'lsp-deferred)

;;;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook #'my-lisp-mode-hook)

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

;; lsp-java uses the Eclipse JDT Language Server:
;; https://github.com/eclipse/eclipse.jdt.ls

(use-package lsp-java
  :hook (java-mode . lsp-deferred)
  :config
  (setq lsp-java-format-settings-url
        "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
  (setq lsp-java-format-settings-profile "GoogleStyle")
  (setq c-basic-offset 2))

(add-hook 'java-mode-hook #'lsp-deferred)

;;;; Javascript (and JSON)

(defun cfclrk/js-mode-hook ()
  "Customize `js-mode'."
  (setq js-indent-level 2))

(add-hook 'js-mode-hook #'cfclrk/js-mode-hook)
(add-hook 'js-mode-hook #'lsp-deferred)

;;;; Lisp

(dolist (hook '(lisp-mode-hook
                lisp-data-mode-hook))
  (add-hook hook #'my-lisp-mode-hook))

;;;; PHP

(use-package php-mode)

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

;;;; Terraform

(use-package terraform-mode
  :hook (terraform-mode . lsp-deferred)
  :init (setq lsp-terraform-server '("terraform-ls" "serve")))

;;;; WebAssembly

(use-package wat-mode
  :straight (wat-mode :type git :host github :repo "devonsparks/wat-mode"))

;;;; YAML

(add-hook 'yaml-mode-hook #'lsp-deferred)

;; Associate schemas with certain files
(setq lsp-yaml-schemas
      '((/Users/chris\.clark/\.config/eksctl/schema\.json . ["/eksctl/cluster.yaml"])))

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

;;; init.el ends here
