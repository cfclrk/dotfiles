;; This is meant to be used with emacs Prelude. Put this file in
;; ~/.emacs.d/personal/init.el

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(require 'prelude-c)
(require 'prelude-clojure)
(require 'prelude-company)
(require 'prelude-css)
(require 'prelude-emacs-lisp)
(require 'prelude-go)
(require 'prelude-haskell)
(require 'prelude-helm)
(require 'prelude-js)
(require 'prelude-lisp)
(require 'prelude-python)
(require 'prelude-rust)
(require 'prelude-shell)
(require 'prelude-xml)
(require 'prelude-yaml)

;;; ----------------------------------------------------------------------------
;;; TODO
;;; ----------------------------------------------------------------------------

;; A Keybinding and function preview current markdown file in browser.
;; Just a passthrough to grip (brew install grip) that errors out nicely if
;; grip is not installed

;; Figure out how to include this in my repo instead of the prelude repo:
;; ((magit-log:magit-log-mode "-n256" "--graph" "--color" "--decorate"))

;;; ----------------------------------------------------------------------------
;;; General
;;; ----------------------------------------------------------------------------

;; customize: don't touch init.el
(setq custom-file (expand-file-name "personal/custm.el" user-emacs-directory))
(load custom-file)

(setq-default fill-column 80)      ; Default line length for text wrapping

(setq mark-ring-max 10             ; Num items to keep in mark ring
      toggle-debug-on-error t      ; Show stack trace on any Emacs error
      ns-pop-up-frames nil         ; Don't open files in new frame
      whitespace-line-column nil   ; Ensure whitespace-mode uses fill-column
      prelude-guru nil             ; Turn off little how-to-use-emacs tips
      prelude-tips nil)            ; Don't show prelude tips at startup

;; ⌘ as Meta and ⎇ as Super on MacOS
(when (eq system-type 'darwin)
  (setq ns-command-modifier 'meta
        ns-alternate-modifier 'super))

; highlight color for marked region
(set-face-attribute 'region nil :background "#7070A0")

; utf-8 stuff
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; M-o to run occur
(define-key prelude-mode-map (kbd "M-o") 'occur)

(setenv "REQUESTS_CA_BUNDLE" "/Users/chris.clark/IronNet/certificates/ironnet_ca_bundle.pem")

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

;; Use a larger font in the mode line
(set-face-attribute 'mode-line nil :height 120)

;;; ----------------------------------------------------------------------------
;;; Packages
;;; ----------------------------------------------------------------------------

;; Packages to install in addition to those already defined in prelude-packages
;; and in each prelude language module at the head of this file.

(prelude-require-packages '(bats-mode
                            blacken
                            clj-refactor
                            company-lsp
                            csv-mode
                            dockerfile-mode
                            fish-mode
                            flycheck-mypy
                            forge
                            geiser
                            github-browse-file
                            helm-descbinds
                            key-chord
                            lsp-mode
                            markdown-mode
                            org
                            org-bullets
                            page-break-lines
                            pipenv
                            py-isort
                            powershell
                            python-pytest
                            racket-mode
                            restclient
                            toml-mode
                            visual-fill-column
                            yapfify
                            yasnippet))

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

;; Render ^L (page break) as a nice line across the buffer
(global-page-break-lines-mode)

;; Theme customization
(custom-theme-set-faces
 'zenburn

 ;; outline-mode
 `(outline-1 ((t (:height 1.1 :foreground "#268bd2" :weight bold))))
 `(outline-2 ((t (:height 1.0 :foreground "#2aa198" :weight bold))))
 `(outline-3 ((t (:foreground "#b58900" :weight bold))))

 ;; org-mode
 `(org-level-1 ((t (:inherit outline-1 :weight bold))))
 `(org-level-2 ((t (:inherit outline-2 :weight bold))))
 `(org-level-3 ((t (:inherit outline-3 :weight bold)))))

;;; ----------------------------------------------------------------------------
;;; Window and frame control
;;; ----------------------------------------------------------------------------

;; TODO: for several of these I'd rather they be in other windows but switch
;; focus to that window automatically.

(setq display-buffer-alist
      '(("\\*compilation\\*" display-buffer-same-window)
        ("\\*cider.*" display-buffer-use-some-window)
        ("magit.*: " display-buffer-use-some-window)
        ("\\*Python\\*" display-buffer-use-some-window)
        ("\\*Help\\*" display-buffer-same-window)
        ("\\*.el.gz" display-buffer-same-window)
        ;("\\*go-guru-output\\*" display-buffer-same-window)
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

(defun my-visual-line-mode-hook ()
  "Set options suitable for writing without newlines."
  (visual-fill-column-mode)

  ;; not sure of a way to explicitly disable
  ;; whitespace mode, only toggle it
  (whitespace-mode 'toggle))

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
  (define-key go-mode-map (kbd "C-h f") 'helm-apropos)
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
                           (setq js2-missing-semi-one-line-override t)
                           ;; TODO shouldn't need this
                           (setq whitespace-line-column 110)))

;; Python
;; ------

(require 'flycheck-mypy)  ; use with M-x flycheck-select-checker
(add-to-list 'auto-mode-alist '("Pipfile\\'" . toml-mode))

(defun my-python-mode-hook ()
  "Customize `python-mode'."

  (setq fill-column 88
        whitespace-line-column 88  ;; bug with whitespace mode not using fill-column
        python-fill-docstring-style 'django
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"))

(add-hook 'python-mode-hook 'my-python-mode-hook)

;; Racket
;; ------

(add-hook 'racket-mode-hook (lambda () (run-hooks 'prelude-lisp-coding-hook)))

;; Rust
;; ----

(add-hook 'rust-mode-hook (lambda ()
                            (set-fill-column 100)
                            ;; TODO shouldn't need this
                            (setq whitespace-line-column 100)))

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

;; gitconfig
(add-to-list 'auto-mode-alist '("gitconfig" . gitconfig-mode))

;; magit
(setq magit-diff-refine-hunk 'all)
(with-eval-after-load 'magit
  (progn
    (require 'forge)
    (add-to-list 'forge-alist '("homegithub" "api.github.com" "github.com" forge-github-repository))))

;; markdown
(setq markdown-command "grip --export -")

;; helm
(require 'prelude-helm-everywhere)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

;; html
(setq flycheck-tidyrc (expand-file-name "~/.config/tidyrc"))
(setq-default flycheck-disabled-checkers '(html-tidy)) ; too noisy

;; key-chord
(key-chord-mode +1)
(setq key-chord-two-keys-delay 0.03
      key-chord-one-key-delay 0.15)
(key-chord-define-global "hy" 'helm-show-kill-ring)
(key-chord-define-global "hu" 'undo-tree-visualize)
(key-chord-define-global "hw" 'avy-goto-word-1)
(key-chord-define-global "hl" 'avy-goto-line)
(key-chord-define-global "hx" 'helm-M-x)

;; make
(add-to-list 'prelude-indent-sensitive-modes 'makefile-bsdmake-mode)

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

;;; ----------------------------------------------------------------------------
;;; Load other files
;;; ----------------------------------------------------------------------------

;; org mode config
(load-file (expand-file-name "personal/org.el" user-emacs-directory))
