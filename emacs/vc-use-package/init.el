;;; init.el -- My emacs setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")))

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

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;;; Packages/Modes
;;  ----------------------------------------------------------------------------

(use-package markdown-xwidget
  :vc (markdown-xwidget
       :url "https://github.com/chuxubank/markdown-xwidget"
       :rev :newest)
  :demand t
  :after markdown-mode
  :custom
  (markdown-xwidget-github-theme (symbol-name (frame-parameter nil 'background-mode)))
  :bind
  (:map markdown-mode-command-map
        ("x" . markdown-xwidget-preview-mode)))
