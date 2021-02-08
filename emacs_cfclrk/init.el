;;; init.el -- My emacs setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Bootstrap Package Management
;;  ----------------------------------------------------------------------------

;; bootstrap straight.el
(setq straight-use-package-by-default t)
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

;; Install use-package
(straight-use-package 'use-package)

;;; Startup
;;  ----------------------------------------------------------------------------

;; I make ~/.emacs.d a symlink to ~/.config/emacs, which itself is a symlink to
;; the actually emacs configuration directory. Follow the symlinks so that I
;; don't pollute ~/.emacs.d/, and so that I can update my ~/.config/emacs
;; symlink without affecting an already-running Emacs. See:
;; https://emacs.stackexchange.com/a/5470/6769
(setq user-emacs-directory (file-truename "~/.emacs.d/"))

;; Number of bytes that can be read from a sub-process in one read operation.
;; Good for dealing with verbose sub-processes like, ehem, an LSP server.
(setq read-process-output-max (* 4 1024 1024)) ;; 4 MiB (default is 8 KiB)

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


;;; Editor General
;;  ----------------------------------------------------------------------------

(tool-bar-mode -1)    ;; No tool bar, which has the save button, etc
(scroll-bar-mode -1)  ;; No scroll bars to the right of buffers
(setq make-backup-files nil      ;; Do not save ~ backup files
      inhibit-splash-screen t)

;;; MacOS

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

;;; Display
;;  ----------------------------------------------------------------------------

;; Use Source Code Pro on MacOS
;; (when (eq system-type 'darwin)
;;   (set-face-attribute 'default nil :family "Source Code Pro"))

;; Use a larger font on big monitors
(when window-system
  (if (> (nth 2 (frame-monitor-attribute 'geometry)) 1600)
      (set-face-attribute 'default nil :height 170)))

;; Use the doom-one theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Use the doom modeline
(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-height 40))

;;; Programming Languages
;;  ----------------------------------------------------------------------------

;;;; Elisp

;; (defun cfc/emacs-lisp-mode-hook ()
;;   (rainbow-delimeters-mode +1))
;; (add-hook 'emacs-lisp-mode-hook 'cfc/emacs-lisp-mode-hook)

;;;; Golang

(use-package go-mode
  :config
  (setq go-test-args "-v"))

;;;; Javascript (and JSON)

(defun cfc/js-mode-hook ()
  (setq js-indent-level 2))
(add-hook 'js-mode-hook 'cfc/js-mode-hook)

;;; Packages
;;  ----------------------------------------------------------------------------

;;;; ace

(use-package ace-window
  :bind ("M-l" . ace-window)
  :config (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

;;;; fish

(use-package fish-mode)

;;;; helpful

(use-package helpful)

;;;; LSP

(use-package lsp-mode
  :hook
  (go-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deferred))

;;;; markdown

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;; git, magit, forge

(use-package magit)

;;;; org

(load-file (expand-file-name "org.el" user-emacs-directory))

;;;; rainbow-delimiters

(use-package rainbow-delimiters)

;;;; selectrum

(use-package selectrum
  :config (selectrum-mode +1))


;;;; setenv-file

(use-package setenv-file
  :straight (setenv-file :type git :host github :repo "cfclrk/setenv-file")
  :config (setq setenv-file-dir (expand-file-name "~/.env/")))

;;;; which-key

(use-package which-key
  :config (which-key-mode +1))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
