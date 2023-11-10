;;; init.el -- My emacs setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(blink-cursor-mode -1)
(set-language-environment "UTF-8")

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default fill-column 80)

(setq help-window-select t
      inhibit-splash-screen t ;; Do not show the welcome page
      make-backup-files nil)  ;; Do not save ~ backup files

;; ⌘ as Meta and ⎇ as Super on MacOS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'hyper))

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;; Packages/Modes
;;  ----------------------------------------------------------------------------

;;;; rainbow-delimiters

(use-package rainbow-delimiters
  :ensure t)

;;; init.el ends here
