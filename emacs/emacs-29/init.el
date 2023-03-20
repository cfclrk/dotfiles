;;; init.el -- My emacs setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Elpaca
;;  ----------------------------------------------------------------------------

(defvar elpaca-installer-version 0.3)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; To install a package defined with use-package, evaluate the use-package form
;; and then run (elpaca-process-queues).

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
(delete-selection-mode t)    ; Typed text replaces selection
(scroll-bar-mode -1)

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

;;; Theme, Font, Display
;;  ----------------------------------------------------------------------------

(defun my/font-installed-p (font-name)
  "Check if font with FONT-NAME is available.
FONT-NAME is a string like 'Roboto Mono'."
  (find-font (font-spec :name font-name)))

;; Set the default font to Roboto Mono
(set-face-attribute 'default nil
                    :family "Roboto Mono"
                    :weight 'normal
                    :height 140)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;;; Functions
;;  ----------------------------------------------------------------------------

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

(defun pprint (form &optional printcharfun)
  "Return a pretty-printed version of FORM.

Optional PRINTCHARFUN is as defined by `princ'."
  (princ (with-temp-buffer
           (cl-prettyprint form)
           (buffer-string))
         printcharfun))

;;; Text
;;  ----------------------------------------------------------------------------

;; whitespace
(require 'whitespace)
(setq whitespace-style '(face tabs empty trailing lines-tail)
      whitespace-line-column nil)
(add-hook 'before-save-hook 'whitespace-cleanup)

;;; Completion
;;  ----------------------------------------------------------------------------

(use-package vertico
  :elpaca (vertico
           :files (:defaults "extensions/*"))
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :elpaca nil
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; Packages/Modes
;;  ----------------------------------------------------------------------------

;;;; ace

(use-package ace-window
  :demand t
  :bind ("M-l" . ace-window)
  :config (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

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

;;;; crux

(use-package crux
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c f" . crux-recentf-find-file)))

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

;;;; env

;;;; helpful

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h C" . helpful-command)))

;;;; occur

;; Always switch focus to the Occur buffer when running occur
(add-hook 'occur-hook
          #'(lambda ()
              (switch-to-buffer-other-window "*Occur*")))

;;;; prog-mode

(add-hook 'prog-mode-hook 'outline-minor-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;;; rainbow-delimiters

(use-package rainbow-delimiters)

;;;; recentf

;; Saves recent file names in $user-emacs-directory/recentf.

(use-package recentf
  :elpaca nil
  :ensure nil
  :config
  (setq recentf-max-saved-items 500
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

;;;; visual-fill-column

(use-package visual-fill-column)

;;; Programming Languages
;;  ----------------------------------------------------------------------------

;;;; Lisp

(defun my/lisp-mode-hook ()
  "General configuration for any LISP."
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (setq fill-column 80)
  ;; Restart whitespace mode so that it properly uses `fill-column'.
  (whitespace-mode -1)
  (whitespace-mode +1))

;;;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook #'my/lisp-mode-hook)
