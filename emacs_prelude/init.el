;;; init.el -- My emacs setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Startup
;;  ----------------------------------------------------------------------------

;; I make ~/.emacs.d a symlink to my Emacs configuration. By following the
;; symlink, I can update my ~/.emacs.d symlink while Emacs is running without
;; affecting an already-running Emacs. See:
;; https://emacs.stackexchange.com/a/5470/6769
(setq user-emacs-directory (file-truename "~/.emacs.d/"))

;; Number of bytes that can be read from a sub-process in one read operation.
;; Good for dealing with verbose sub-processes like, ehem, an LSP server.
(setq read-process-output-max (* 4 1024 1024)) ;; 4 MiB (default is 8 KiB)

(defun my-startup-hook ()
  "Show me some Emacs startup stats."
  (message "*** Emacs loaded in %s with %d garbage collections."
           (format "%.1f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook 'my-startup-hook)

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/") ; http instead of https
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(require 'prelude-c)
(require 'prelude-clojure)
(require 'prelude-company)
(require 'prelude-css)
(require 'prelude-emacs-lisp)
(require 'prelude-go)
(require 'prelude-ivy)
(require 'prelude-js)
(require 'prelude-lisp)
(require 'prelude-lsp)
(require 'prelude-rust)
(require 'prelude-shell)
(require 'prelude-xml)
(require 'prelude-yaml)

(setq package-pinned-packages
      '((org . "org")))

(prelude-require-packages '(all-the-icons
                            all-the-icons-ivy-rich
                            bats-mode
                            csv-mode
                            dap-mode
                            dockerfile-mode
                            doom-themes
                            doom-modeline
                            emmet-mode
                            fish-mode
                            flycheck-package
                            geiser
                            github-browse-file
                            ivy-rich
                            lsp-mode
                            racket-mode
                            restclient
                            terraform-mode
                            toml-mode
                            use-package
                            visual-fill-column
                            writeroom-mode
                            yasnippet))

;;; General
;;  ----------------------------------------------------------------------------

;; So that I don't have to say ":ensure t" in every declaration
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq-default fill-column 80)      ; Default line length for text wrapping
(setq mark-ring-max 10             ; Num items to keep in mark ring
      ns-pop-up-frames nil         ; Don't open files in new frame
      prelude-guru nil             ; Turn off little how-to-use-emacs tips
      prelude-tips nil             ; Don't show prelude tips at startup
      help-window-select t         ; Always select a help window when it opens
      history-delete-duplicates t) ; Don't keep duplicate commands in history

;; ⌘ as Meta and ⎇ as Super on MacOS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super))

;; Keep marked regions highlighted in non-selected windows
(setq highlight-nonselected-windows t)

(set-language-environment "UTF-8")

;; M-x what-cursor-position (C-x =) is like a lightweight describe-char. This
;; adds the char name.
(setq what-cursor-show-names t)

;; Make whitespace-mode use `fill-column'
(setq-default whitespace-line-column nil)

;; Remap C-h g from `describe-gnu-project' to `github-browse-file'
(global-set-key (kbd "C-h g") 'github-browse-file)

;; Not sure I really want this yet
(setq vc-follow-symlinks t)

;;; Keymaps
;;  ----------------------------------------------------------------------------

;; Rebind C-t from `transpose-chars' to `recenter-top-bottom'. I don't think
;; I've ever used `transpose-chars' except by accident.
(global-set-key (kbd "C-t") 'recenter-top-bottom)

;; TODO

;; M-g l -> go to line. Get rid of M-g M-n and M-g M-p.
;; New keymap C-r for rectangle commands. Unbind all rectangle commands in C-x r.

;;; Fonts
;;  ----------------------------------------------------------------------------

;; Use Source Code Pro on MacOS
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Source Code Pro"))

;; Use a larger font on bigger screens
(when window-system
  (if (> (nth 2 (frame-monitor-attribute 'geometry)) 1600)
      (set-face-attribute 'default nil :height 170)))

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :family "Source Sans Pro"
                    :height 245
                    :weight 'normal
                    :width 'normal)

;;; My functions
;;  ----------------------------------------------------------------------------

(defun cfc/set-font-size (font-size)
  "Set font height to the given FONT-SIZE.

Get the current font height:

    (face-attribute 'default :height)

TODO: display current font size in prompt."
  (interactive "nFont Size: ")
  (let ((frame-inhibit-implied-resize t))
    (set-face-attribute 'default nil :height font-size)
    (set-face-attribute 'mode-line nil :height font-size)))

(defun cfc/kill-all-other-buffers ()
  "Kill all buffers other than current buffer."
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(defun cfc/show-buffer-file-name ()
  "Copy and display the full path to the current file.

  You can also do this with \\[easy-kill-on-buffer-file-name] (M-w b)"
  (interactive)
  (message (buffer-file-name))
  (kill-new (buffer-file-name)))

;; C-c z to see full path of file in the current buffer
(global-set-key (kbd "C-c z") 'cfc/show-buffer-file-name)

;;; Cosmetics
;;  ----------------------------------------------------------------------------

;; Remove some UI features
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Doom modeline, oooooh yeah!
(doom-modeline-mode 1)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-height 40)

;; Render ^L (page break) as a nice line across the buffer
;; (prelude-require-package 'page-break-lines)
;; (global-page-break-lines-mode)

;;; window and Frame Control
;;  ----------------------------------------------------------------------------

;; TODO: for several of these I'd rather they be in other windows but switch
;; focus to that window automatically.

(setq display-buffer-alist
      '(("\\*compilation\\*" display-buffer-same-window)
        ("\\*cider.*" display-buffer-use-some-window)
        ("magit.*: " display-buffer-use-some-window)
        ("\\*Python\\*" display-buffer-use-some-window)
        ("\\*Help\\*" display-buffer-same-window)
        ("\\*Go .*" display-buffer-same-window)
        ("\\*godoc*" display-buffer-same-window)))

(add-hook 'occur-hook
          '(lambda ()
             (switch-to-buffer-other-window "*Occur*")))

(setq split-height-threshold 100)

;;; WIP: Scratch text buffer
;;  ----------------------------------------------------------------------------

;; Keybinding/function to create a *scratch-text* buffer, not backed by a file
;; (see generate-new-buffer)

;; Make C-a and C-e go to beginning/end of visual line instead of beginning of
;; actual line

;; Spell check

;; Thesaurus for word at point

;; See:
;; https://github.com/hlissner/doom-emacs/tree/develop/modules/ui/zen
;; https://github.com/hlissner/doom-emacs/tree/develop/modules/checkers

;;; WIP: CloudFormation Mode
;;  ----------------------------------------------------------------------------

;; Disable flyspell (or load a dictionary with all possible keys?)
;; Use cloudformation yasnippets
;; YAML schema validation?
;;  - What about SAM templates or transformations?
;; cfn-lint on save?

(defun my-visual-line-mode-hook ()
  "Set options suitable for writing without newlines."
  (visual-fill-column-mode))

;; TODO: Review these modes. Disable visual-fill-column-mode when visual-line
;; mode is disabled.
;; (add-hook 'visual-line-mode-hook 'my-visual-line-mode-hook)

;;; Programming languages
;;  ----------------------------------------------------------------------------

;;;; Clojure and cider

(defun my-clojure-mode-hook ()
  "Customize `clojure-mode'."
  (prelude-require-packages '(clj-refactor))
  (setq cider-repl-use-pretty-printing t))

(setq prelude-clojure-mode-hook '(my-clojure-mode-hook
                                  lisp-smartparens-hook))

;;;; Elisp

(defun my-emacs-lisp-mode-hook ()
  "Customize emacs-lisp mode."
  (outline-minor-mode)
  (setq sentence-end-double-space nil))

(setq prelude-emacs-lisp-mode-hook '(prelude-emacs-lisp-mode-defaults
                                     my-emacs-lisp-mode-hook
                                     lisp-smartparens-hook
                                     outline-minor-mode))

;;;; Fish

(add-hook 'fish-mode-hook (lambda () (setq tab-width 4)))

;;;; Golang

(defun my-go-mode-hook ()
  "Customize `go-mode'."
  (setq tab-width 4
        go-test-args "-v")

  ;; Don't highlight lines longer than fill-column.Mnually toggle with:
  ;; "M-x whitespace-toggle-options L"
  (whitespace-toggle-options '(lines-tail))

    ;; I don't like that prelude overrode C-h f to run godoc-at-point
    (define-key go-mode-map (kbd "C-h f") 'counsel-describe-function)
    (global-set-key (kbd "s-g") 'godoc-at-point)

  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; (defun my-go-mode-hook ()
;;   "Customize `go-mode'."

;;   (setq tab-width 4
;;         fill-column 80
;;         go-test-args "-v")

;;   (global-set-key (kbd "M-.") 'godef-jump)
;;   (global-set-key (kbd "M-p") 'godef-jump-other-window)

;;   ;; Load yasnippets
;;   (let ((d (expand-file-name "snippets/yasnippet-go" user-emacs-directory)))
;;     (add-to-list 'yas-snippet-dirs d))

;;   ;; Requires running go get -u github.com/zmb3/gogetdoc
;;   (setq godoc-at-point-function 'godoc-gogetdoc)

;;   ;; I don't like that prelude overrode C-h f to run godoc-at-point
;;   ;(define-key go-mode-map (kbd "C-h f") 'helm-apropos)
;;   (global-set-key (kbd "s-g") 'godoc-at-point)

;;   (whitespace-toggle-options '(lines-tail)))

;; ;; Run my-go-mode-hook after the prelude go mode hook, because my hook overrides
;; ;; some things in the prelude hook.
;; (setq prelude-go-mode-hook '(prelude-go-mode-defaults
;;                              yas-minor-mode
;;                              go-guru-hl-identifier-mode
;;                              my-go-mode-hook))

;;(add-hook 'go-mode-hook 'my-go-mode-hook)

;;;; Javascript

(defun my-js2-mode-hook ()
  "Customize `js2-mode'."
  (setq js-indent-level 2
        js2-strict-missing-semi-warning nil
        js2-missing-semi-one-line-override t))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

;;;; Python

(add-to-list 'auto-mode-alist '("Pipfile\\'" . toml-mode))

(projectile-register-project-type 'python-cfclrk '("setup.py")
                                  :project-file "setup.py"
                                  :src-dir "src"
                                  :test-dir "tests"
                                  :compile "make dev"
                                  :test "make test"
                                  :test-prefix "test"
                                  :test-suffix"_test")

(defun my-python-mode-hook ()
  "Customize `python-mode'."
  (prelude-require-packages '(pipenv
                              pyenv-mode
                              python-pytest))

  (setq python-pytest-executable "pytest -s")

  ;; LSP using the pyls (Palantir) language server. The Microsoft one sucks.
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)
     ("pyls.plugins.yapf.enabled" nil t)
     ("pyls.plugins.pydocstyple.enabled" t t)

     ;; Disable these as they duplicate flake8 functionality
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))

  (setq lsp-pyls-plugins-flake8-enabled t)

  (setq fill-column 88
        python-fill-docstring-style 'pep-257-nn
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i")

  ;; Restart whitespace-mode so that it properly uses `fill-column'
  (whitespace-mode -1)
  (whitespace-mode +1)

  (subword-mode +1)
  (eldoc-mode +1)

  (require 'pyenv-mode))

(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'python-mode-hook #'lsp-deferred)

;; ;; Set pyenv version whenever switching projects with C-c p p.
;; (defun projectile-pyenv-mode-set ()
;;   "Set pyenv version matching project name."
;;   (let ((project (projectile-project-name)))
;;     (if (member project (pyenv-mode-versions))
;;         (pyenv-mode-set project)
;;       (pyenv-mode-unset))))
;; (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)

;; TODO: get rid of this. For some reason my setup.cfg stuff is not being used
;; correctly.
;; (setq flycheck-python-mypy-args "--ignore-missing-imports")

;;;; Racket

(add-hook 'racket-mode-hook (lambda () (run-hooks 'prelude-lisp-coding-hook)))

;;;; Rust

(add-hook 'rust-mode-hook (lambda ()
                            (set-fill-column 100)))

;;;; Typescript

(prelude-require-package 'typescript-mode)

(defun my-typescript-mode-hook ()
  "Customize `typescript-mode'."
  (setq typescript-indent-level 2))

(add-hook 'typescript-mode-hook #'lsp-deferred 0)
(add-hook 'typescript-mode-hook 'my-typescript-mode-hook 1)

;;; Modes
;;  ----------------------------------------------------------------------------

;;;; avy

(global-set-key (kbd "M-l") 'ace-window)
(setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s))

;;;; aws

(add-to-list 'auto-mode-alist '("credentials\\'" . conf-mode))


;;;; bicycle

(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle)
              ([S-tab] . bicycle-cycle-global)))

(add-hook 'prog-mode-hook 'outline-minor-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;;;; company

(prelude-require-packages '(company))

(setq company-global-modes '(not org-mode))
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)

;;;; dired

(define-key prelude-mode-map (kbd "C-c d") 'dired-jump-other-window)
(setq dired-dwim-target t
      dired-listing-switches "-alh")

;;;; eshell

(use-package eshell-git-prompt)

;;;; flycheck

(setq flycheck-emacs-lisp-load-path 'inherit)

;;;; git

(prelude-require-packages '(forge
                            gitconfig-mode))

(setq magit-diff-refine-hunk 'all)

(with-eval-after-load 'magit
  (require 'forge)
  (add-to-list 'forge-alist '("github-home"
                              "api.github.com"
                              "github.com"
                              forge-github-repository))
  (add-to-list 'forge-alist '("github-work"
                              "api.github.com"
                              "github.com"
                              forge-github-repository))
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

;;;; helpful

(prelude-require-package 'helpful)

(setq counsel-describe-function-function #'helpful-callable
      counsel-describe-variable-function #'helpful-symbol)

;; By default, C-h C is bound to describe `describe-coding-system'
(global-set-key (kbd "C-h C") #'helpful-command)
(global-set-key (kbd "C-h k") #'helpful-key)

;;;; HTML

(setq flycheck-tidyrc (expand-file-name "~/.config/tidyrc"))
(setq-default flycheck-disabled-checkers '(html-tidy)) ; too noisy

;;;; ivy/counsel/swiper
(setq ivy-count-format "(%d/%d) "
      ivy-height 20
      ivy-wrap t)

;; Fun icons
(all-the-icons-ivy-rich-mode t)
(ivy-rich-mode t)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

;; Don't start all M-x searches with "^"
(setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) "")

;;;; JSON and jq

(prelude-require-package 'counsel-jq)

(defun my-json-mode-hook ()
  "Customize `json-mode'."
  (setq-local js-indent-level 2
              js2-basic-offset 2))

(add-hook 'json-mode-hook 'my-json-mode-hook)

;;;; key-chord

(prelude-require-package 'key-chord)
(require 'key-chord)

(key-chord-mode +1)
(setq key-chord-one-key-delay 0.05)
(key-chord-define-global "\\a" 'avy-goto-word-1)
(key-chord-define-global "\\o" 'lsp-format-buffer)
(key-chord-define-global "\\e" 'org-toggle-inline-images)
(key-chord-define-global "\\u" 'undo-tree-visualize)

;;;; LSP

;; (defun my-lsp-mode-hook ()
;;   "Customize `lsp-mode'."
;;   (lsp-enable-which-key-integration))
;; (add-hook 'lsp-mode-hook 'my-lsp-mode-hook)

(add-hook 'lsp-ui-mode-hook '(lambda ()
                               (setq lsp-ui-doc-position 'bottom)))

(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

;;;; make

(add-to-list 'prelude-indent-sensitive-modes 'makefile-bsdmake-mode)
(add-to-list 'prelude-indent-sensitive-modes 'snippet-mode)

;;;; markdown

;; On MacOS, this requires building Emacs with xwidgets enabled
(use-package grip-mode
  :config (setq grip-update-after-change nil
                grip-github-user (getenv "GITHUB_USER")
                grip-github-password (getenv "GITHUB_TOKEN"))
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

(defun my-markdown-mode-hook ()
  "Customize `markdown-mode'."
  (whitespace-toggle-options '(lines-tail))
  (auto-fill-mode 1))
(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

;;;; projectile

(add-to-list 'projectile-globally-ignored-directories "*.mypy_cache")
(add-to-list 'projectile-globally-ignored-directories "*.pytest_cache")
(add-to-list 'projectile-globally-ignored-directories "*logs")
(add-to-list 'projectile-globally-ignored-directories "*_output")
;;(add-to-list 'projectile-globally-ignored-directories ".terraform")

;; Update projectile project detection to identify projects by the files in
;; `projectile-project-root-files'. Originally, projectile identifies projects
;; using a different set of files markers. Original value was:
;;
;;    (".projectile" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs")
;;
;;  Not sure if this is a good idea, trying it out. So far so good.

;; TODO: Projectile discovers projects by iterating through
;; projectile-project-root-files-bottom-up and checking each parent dir for that
;; file. It searches the entire dir lineage for the first item, then the next.
;; Instead, it should iterate through the whole list at each dir level.
(setq projectile-project-root-files-bottom-up
      (cons "Makefile" projectile-project-root-files))

;;;; setenv-file

(let ((setenv-file-proj (expand-file-name "~/Projects/elisp/setenv-file/")))
  (load (f-join setenv-file-proj "setenv-file.el"))
  (add-to-list 'Info-directory-list (f-join setenv-file-proj "doc/"))
  (setq setenv-file-dir (expand-file-name "~/.env/")))

;;;; swiper

(global-set-key (kbd "C-s") 'swiper-isearch)

;;;; smartparens

(defun lisp-smartparens-hook ()
  "Smartparens settings to apply in Lisp modes."

  ;; wrapping
  (define-prefix-command 'sp-wrap-key-map)
  (define-key sp-wrap-key-map (kbd "a") 'sp-wrap-round) ; mneumonic: "around"
  (define-key sp-wrap-key-map (kbd "u") 'sp-unwrap-sexp)
  (define-key sp-wrap-key-map (kbd "c") 'sp-wrap-curly)
  (define-key sp-wrap-key-map (kbd "r") 'sp-rewrap-sexp)
  (define-key smartparens-mode-map (kbd "M-r") sp-wrap-key-map)

  ;; slurping and barfing
  (define-key smartparens-mode-map (kbd "S-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "S-<left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-S-<right>") 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-S-<left>") 'sp-backward-slurp-sexp)

  ;; splicing
  (define-prefix-command 'splice-key-map)
  (define-key splice-key-map (kbd "s") 'sp-splice-sexp)
  (define-key splice-key-map (kbd "f") 'sp-splice-sexp-killing-forward)
  (define-key splice-key-map (kbd "b") 'sp-splice-sexp-killing-backward)
  (define-key splice-key-map (kbd "a") 'sp-splice-sexp-killing-around)
  (define-key smartparens-mode-map (kbd "M-s") splice-key-map)

  ;; selection
  (define-prefix-command 'sp-select-key-map)
  (define-key sp-select-key-map (kbd "n") 'sp-select-next-thing)
  (define-key sp-select-key-map (kbd "p") 'sp-select-previous-thing-exchange)
  (define-key smartparens-mode-map (kbd "M-c") sp-select-key-map)

  ;; movement
  (define-key smartparens-mode-map (kbd "M-<down>") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "M-<up>") 'sp-backward-up-sexp)
  (define-key smartparens-mode-map (kbd "M-f") 'sp-next-sexp)
  (define-key smartparens-mode-map (kbd "M-b") 'sp-previous-sexp)
  (define-key smartparens-mode-map (kbd "M-a") 'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "M-e") 'sp-end-of-sexp))

;;;; which-key

(setq which-key-show-early-on-C-h t)
(setq which-key-idle-delay 0.5)

;;;; yasnippet

(require 'yasnippet)
(setq yas-indent-line 'fixed) ;; probably do this in a yaml-mode-hook
;(load (f-join user-emacs-directory "snippets/aws-snippets/aws-snippets.el"))
(yas-global-mode t)

;;; init.el ends here
