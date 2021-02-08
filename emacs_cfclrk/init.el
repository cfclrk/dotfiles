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

;; Do not show splash screen
(setq inhibit-splash-screen t)

;;; MacOS

;; ⌘ as Meta and ⎇ as Super on MacOS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'hyper))

;;; Editor General
;;  ----------------------------------------------------------------------------

;; Hide the tool bar, which has the save button, etc
(tool-bar-mode -1)

;; Do not save ~ backup files
(setq make-backup-files nil)

;;; Display
;;  ----------------------------------------------------------------------------

;; Use Source Code Pro on MacOS
;; (when (eq system-type 'darwin)
;;   (set-face-attribute 'default nil :family "Source Code Pro")

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

;;; Packages
;;  ----------------------------------------------------------------------------

;;;; ace

(use-package ace-window
  :config
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  (global-set-key (kbd "M-l") 'ace-window))

;;;; helpful

(use-package helpful)

;;;; magit

(use-package magit)

;;;; selectrum

(use-package selectrum)
(selectrum-mode +1)

;;;; setenv-file

(use-package setenv-file
  :straight (setenv-file :type git :host github :repo "cfclrk/setenv-file")
  :config (setq setenv-file-dir (expand-file-name "~/.env/")))

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
