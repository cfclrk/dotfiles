;; This is meant to be used with emacs Prelude. Put this file in
;; ~/.emacs.d/personal/init.el

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
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
(require 'prelude-ruby)
(require 'prelude-rust)
(require 'prelude-shell)
(require 'prelude-scss)
(require 'prelude-xml)
(require 'prelude-yaml)

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

;; C-c d to open dired for the current directory
(define-key prelude-mode-map (kbd "C-c d") 'dired-jump)

;; M-o to run occur
(define-key prelude-mode-map (kbd "M-o") 'occur)

;;; ----------------------------------------------------------------------------
;;; Font
;;; ----------------------------------------------------------------------------

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Use a larger font in the mode line
(set-face-attribute 'mode-line nil :height 130)

;; Use a larger font on huge monitors
(when window-system
  (if (> (x-display-pixel-width) 1600)
      (set-face-attribute 'default nil :height 130)))


;;; ----------------------------------------------------------------------------
;;; Packages
;;; ----------------------------------------------------------------------------

;; Packages to install in addition to those already defined in prelude-packages
;; and in each prelude language module at the head of this file.

(prelude-require-packages '(bats-mode
                            clj-refactor
                            csv-mode
                            dockerfile-mode
                            exec-path-from-shell
                            fish-mode
                            flycheck-mypy
                            geiser
                            helm-descbinds
                            key-chord
                            markdown-mode
                            org-bullets
                            page-break-lines
                            powershell
                            racket-mode
                            restclient
                            toml-mode
                            visual-fill-column
                            yapfify))

;;; ----------------------------------------------------------------------------
;;; Useful functions
;;; ----------------------------------------------------------------------------

(defun cfc/kill-all-other-buffers ()
  "Kill all buffers other than current buffer."
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(defun cfc/show-buffer-file-name ()
  "Show the full path to the file in this buffer."
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
        ("\\*Python\\*" display-buffer-same-window)
        ("\\*Help\\*" display-buffer-same-window)
        ("\\*.el.gz" display-buffer-same-window)
        ;("\\*go-guru-output\\*" display-buffer-same-window)
        ("\\*Go .*" display-buffer-same-window)
        ("\\*godoc .*" display-buffer-same-window)))

(add-hook 'occur-hook
          '(lambda ()
             (switch-to-buffer-other-window "*Occur*")))

(setq split-height-threshold 100)

;;; ----------------------------------------------------------------------------
;;; Programming languages
;;; ----------------------------------------------------------------------------

;; Clojure and cider
;; -----------------

(setq cider-repl-use-pretty-printing t)

(defun my-clojure-mode-hook ()
  "Customize clojure-mode."
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook 'my-clojure-mode-hook)

;; Elisp
;; -----

(defun my-emacs-lisp-mode-hook ()
  "Customize emacs-lisp mode."
  (setq sentence-end-double-space nil))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;; Fish
;; ----

(add-hook 'fish-mode-hook (lambda () (setq tab-width 4)))

;; Golang
;; ------

(defun my-go-mode-hook ()
  "Customize go-mode."

  (setq tab-width 4
        fill-column 80
        go-test-args "-v"
        company-mode t)

  (global-set-key (kbd "M-.") 'godef-jump-other-window)

  ;; Don't highlight lines longer than fill-column
  ;; Mnually toggle with "M-x whitespace-toggle-options L"
  (whitespace-toggle-options '(lines-tail))

  ;; I don't like that prelude overrode C-h f to run godoc-at-point
  (define-key go-mode-map (kbd "C-h f") 'helm-apropos)
  (global-set-key (kbd "s-g") 'godoc-at-point))

;; Run my-go-mode-hook after the prelude go mode hook, because my hook overrides
;; some things in the prelude hook.
(setq prelude-go-mode-hook '(prelude-go-mode-defaults
                             my-go-mode-hook
                             go-guru-hl-identifier-mode))

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
                           ;; TODO shouldn't need this
                           (setq whitespace-line-column 110)))

;; Python
;; ------

(require 'flycheck-mypy)  ; use with M-x flycheck-select-checker

(defun my-python-mode-hook ()
  (setq fill-column 110
        whitespace-line-column 110  ;; bug with whitespace mode not using fill-column
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

;; helm
(require 'prelude-helm-everywhere)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

;; key-chord
(key-chord-mode +1)
(setq key-chord-two-keys-delay 0.03
      key-chord-one-key-delay 0.15)
(key-chord-define-global "hy" 'helm-show-kill-ring)
(key-chord-define-global "hu" 'undo-tree-visualize)
(key-chord-define-global "hw" 'avy-goto-word-1)
(key-chord-define-global "hl" 'avy-goto-line)
(key-chord-define-global "hx" 'helm-M-x)

;; smartparens

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
;; (remove? So far I never use these)
(define-key smartparens-mode-map (kbd "M-<down>") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "M-<up>") 'sp-backward-up-sexp)

;; In non-lisp modes, I'd rather have this sp-forward-sexp and sp-backward-sexp
;; TODO: add this to a  lisp mode hook
(define-key smartparens-mode-map (kbd "M-f") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "M-b") 'sp-previous-sexp)

(define-key smartparens-mode-map (kbd "M-a") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "M-e") 'sp-end-of-sexp)

;;; ----------------------------------------------------------------------------
;;; Load other files
;;; ----------------------------------------------------------------------------

;; org mode config
(load-file (expand-file-name "personal/org.el" user-emacs-directory))
