;;; init.el -- My emacs setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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
(require 'prelude-haskell)
(require 'prelude-ivy)
(require 'prelude-js)
(require 'prelude-lisp)
(require 'prelude-python)
(require 'prelude-rust)
(require 'prelude-shell)
(require 'prelude-xml)
(require 'prelude-yaml)

;;; ----------------------------------------------------------------------------
;;; General
;;; ----------------------------------------------------------------------------

;; When I compile Emacs myself and run from /Applications, this is set to "/".
;; How is this set in the publicly distributed dmg emacs?
;(setq default-directory (expand-file-name "~/"))

;; [Emacs 27] Number of bytes that can be read from a sub-process in one read
;; operation. Good for dealing with high-throughput sub-processes like, ehem, an
;; LSP server.
(setq read-process-output-max (* 1024 1024)) ;; 1 MiB (default is 4 KiB)

(setq-default fill-column 80)      ; Default line length for text wrapping

(setq mark-ring-max 10             ; Num items to keep in mark ring
      toggle-debug-on-error t      ; Show stack trace on any Emacs error
      ns-pop-up-frames nil         ; Don't open files in new frame
      prelude-guru nil             ; Turn off little how-to-use-emacs tips
      prelude-tips nil             ; Don't show prelude tips at startup
      help-window-select t         ; Always select a help window when it opens
      history-delete-duplicates t) ; Don't keep duplicate commands in history

;; ⌘ as Meta and ⎇ as Super on MacOS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super))

;; keep marked regions highlighted in non-selected windows
(setq highlight-nonselected-windows t)

;; UTF-8 stuff - check if I can remove this now
(set-language-environment "UTF-8")
;; (prefer-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
(setq what-cursor-show-names t)

;; M-o to run occur
(define-key prelude-mode-map (kbd "M-o") 'occur)

;; Make whitespace-mode use `fill-column'
(setq-default whitespace-line-column nil)

;;; ----------------------------------------------------------------------------
;;; Packages
;;; ----------------------------------------------------------------------------

;; Packages to install in addition to those already defined in prelude-packages
;; and in each prelude language module at the head of this file.

(prelude-require-packages '(all-the-icons
                            bats-mode
                            blacken
                            clj-refactor
                            company-lsp
                            csv-mode
                            dockerfile-mode
                            doom-modeline
                            doom-themes
                            emmet-mode
                            fish-mode
                            flycheck-mypy
                            flycheck-package
                            forge
                            geiser
                            github-browse-file
                            key-chord
                            lsp-mode
                            page-break-lines
                            pipenv
                            py-isort
                            powershell
                            python-pytest
                            racket-mode
                            restclient
                            toml-mode
                            visual-fill-column
                            writeroom-mode
                            yapfify
                            yasnippet))

;;; ----------------------------------------------------------------------------
;;; Font
;;; ----------------------------------------------------------------------------

;; Source Code Pro is nice in MacOS but not Ubuntu
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height 120
                      :weight 'normal
                      :width 'normal))

;; Use a larger font on bigger screens
;; TODO: x-display-pixel-width doesn't seem to work (anymore?). Maybe try:
;;   (nth 4 (assq 'geometry (car (display-monitor-attributes-list))))
(when window-system
  (if (> (x-display-pixel-width) 1600)
      (set-face-attribute 'default nil :height 160)))

;; Use a larger font in the mode line
;(set-face-attribute 'mode-line nil :height 120)

;;; ----------------------------------------------------------------------------
;;; Useful functions
;;; ----------------------------------------------------------------------------

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


;;; ----------------------------------------------------------------------------
;;; Cosmetics
;;; ----------------------------------------------------------------------------

;; Remove some UI features
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Doom modeline, oooooh yeah!
(doom-modeline-mode 1)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-height 40)

;; Render ^L (page break) as a nice line across the buffer
(global-page-break-lines-mode)

;;; ----------------------------------------------------------------------------
;;; window and frame control
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; Scratch text buffer (WIP)
;;; ----------------------------------------------------------------------------

;; Keybinding/function to create a *scratch-text* buffer, not backed by a file
;; (see generate-new-buffer)

;; Make C-a and C-e go to beginning/end of visual line instead of beginning of
;; actual line

;; Spell check

;; Thesaurus for word at point

;; See:
;; https://github.com/hlissner/doom-emacs/tree/develop/modules/ui/zen
;; https://github.com/hlissner/doom-emacs/tree/develop/modules/checkers

(defun my-visual-line-mode-hook ()
  "Set options suitable for writing without newlines."
  (visual-fill-column-mode))

(add-hook 'visual-line-mode-hook 'my-visual-line-mode-hook)

;;; ----------------------------------------------------------------------------
;;; Programming languages
;;; ----------------------------------------------------------------------------

;; Clojure and cider
;; -----------------

(setq cider-repl-use-pretty-printing t)
(add-hook 'clojure-mode-hook 'lisp-smartparens-hook)

;; Elisp
;; -----

(defun my-emacs-lisp-mode-hook ()
  "Customize emacs-lisp mode."

  (setq sentence-end-double-space nil))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'lisp-smartparens-hook)

;; Fish
;; ----

(add-hook 'fish-mode-hook (lambda () (setq tab-width 4)))

;; Golang
;; ------

(defun my-go-mode-hook ()
  "Customize `go-mode'."

  (setq tab-width 4
        fill-column 80
        go-test-args "-v")

  (global-set-key (kbd "M-.") 'godef-jump)
  (global-set-key (kbd "M-p") 'godef-jump-other-window)

  ;; Load yasnippets
  (let ((d (expand-file-name "snippets/yasnippet-go" user-emacs-directory)))
    (add-to-list 'yas-snippet-dirs d))

  ;; Requires running go get -u github.com/zmb3/gogetdoc
  (setq godoc-at-point-function 'godoc-gogetdoc)

  ;; I don't like that prelude overrode C-h f to run godoc-at-point
  ;(define-key go-mode-map (kbd "C-h f") 'helm-apropos)
  (global-set-key (kbd "s-g") 'godoc-at-point)

  ;; Don't highlight lines longer than fill-column
  ;; Note: Mnually toggle with "M-x whitespace-toggle-options L"
  (whitespace-toggle-options '(lines-tail)))

;; Run my-go-mode-hook after the prelude go mode hook, because my hook overrides
;; some things in the prelude hook.
(setq prelude-go-mode-hook '(prelude-go-mode-defaults
                             yas-minor-mode
                             go-guru-hl-identifier-mode
                             my-go-mode-hook))

;; Haskell
;; -------

(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))

;; Javascript
;; ----------

(setq js-indent-level 2)
(setq json-reformat:indent-width 2)
(add-hook 'js2-mode-hook (lambda ()
                           (set-fill-column 110)
                           (setq js2-strict-missing-semi-warning nil)
                           (setq js2-missing-semi-one-line-override t)))

;; Python
;; ------

(require 'flycheck-mypy)  ; use with M-x flycheck-select-checker
(add-to-list 'auto-mode-alist '("Pipfile\\'" . toml-mode))

(defun my-python-mode-hook ()
  "Customize `python-mode'."

  (setq fill-column 88
        python-fill-docstring-style 'pep-257-nn
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i")

  ;; Restart whitespace-mode so that it properly uses `fill-column'
  (whitespace-mode -1)
  (whitespace-mode +1))

(add-hook 'python-mode-hook 'my-python-mode-hook)

;; TODO: get rid of this. For some reason my setup.cfg stuff is not being used
;; correctly.
(setq flycheck-python-mypy-args "--ignore-missing-imports")

;; Racket
;; ------

(add-hook 'racket-mode-hook (lambda () (run-hooks 'prelude-lisp-coding-hook)))

;; Rust
;; ----

(add-hook 'rust-mode-hook (lambda ()
                            (set-fill-column 100)))

;;; ----------------------------------------------------------------------------
;;; Modes
;;; ----------------------------------------------------------------------------

;; avy
(global-set-key (kbd "M-l") 'ace-window)
(setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s))

;; aws
(add-to-list 'auto-mode-alist '("credentials\\'" . conf-mode))

;; company
(setq company-global-modes '(not org-mode))

;; dired
(define-key prelude-mode-map (kbd "C-c d") 'dired-jump-other-window)
(setq dired-dwim-target t
      dired-listing-switches "-alh")

;; flycheck
(setq flycheck-emacs-lisp-load-path 'inherit)

;; gitconfig
(add-to-list 'auto-mode-alist '("gitconfig" . gitconfig-mode))

;; magit
(setq magit-diff-refine-hunk 'all)
(with-eval-after-load 'magit
  ;; forge
  (require 'forge)
  (add-to-list 'forge-alist '("github-home"
                              "api.github.com"
                              "github.com"
                              forge-github-repository))

  ;; automatically refresh the magit buffer after saving a file
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

;; html
(setq flycheck-tidyrc (expand-file-name "~/.config/tidyrc"))
(setq-default flycheck-disabled-checkers '(html-tidy)) ; too noisy

;; ivy
(setq ivy-height 20
      ivy-wrap t)
(setq ivy-count-format "(%d/%d) ")

;; key-chord
(key-chord-mode +1)
(setq key-chord-two-keys-delay 0.02
      key-chord-one-key-delay 0.15)
(key-chord-define-global "hu" 'undo-tree-visualize)
(key-chord-define-global "pb" 'blacken-buffer)

;(key-chord-define-global "et" 'helm-show-kill-ring)
;(key-chord-define-global "on" 'helm-all-mark-rings)
(key-chord-define-global "vw" 'avy-goto-word-1)
;(key-chord-define-global "al" 'avy-goto-line)

;; make
(add-to-list 'prelude-indent-sensitive-modes 'makefile-bsdmake-mode)

;; projectile
(add-to-list 'projectile-globally-ignored-directories "*.mypy_cache")
(add-to-list 'projectile-globally-ignored-directories "*.pytest_cache")
(add-to-list 'projectile-globally-ignored-directories "*logs")
(add-to-list 'projectile-globally-ignored-directories "*_output")
;;(add-to-list 'projectile-globally-ignored-directories ".terraform")

;; setenv-file
(load (expand-file-name "~/Projects/elisp/setenv-file/setenv-file.el"))
(add-to-list 'Info-directory-list (expand-file-name "~/Projects/elisp/setenv-file/doc/"))
(setq setenv-file-dir (expand-file-name "~/.env/"))

;; smartparens
(defun lisp-smartparens-hook ()
  "Extra smartparens settings to apply in Lisp modes."

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


;;; init.el ends here
