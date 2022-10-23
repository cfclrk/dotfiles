;;; init.el -- My emacs setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Straight
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
(setq straight-use-package-by-default t)

;;; Editor General
;;  ----------------------------------------------------------------------------

(setq inhibit-splash-screen t ;; Do not show the welcome page
      make-backup-files nil)  ;; Do not save ~ backup files

;; ⌘ as Meta and ⎇ as Super on MacOS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'hyper))

;;; Packages and Modes
;;  ----------------------------------------------------------------------------

;;;; LSP

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  ;; TODO: This doesn't work at all. See some examples:
  ;; - https://github.com/emacs-lsp/lsp-mode/issues/1085
  (dolist (r '("[/\\\\]\\.ijwb\\'"
               "[/\\\\]\\.log\\'"
               "[/\\\\]\\bazel-bin\\'"
               "[/\\\\]\\bazel-testlogs\\'"
               "[/\\\\]\\bazel-out\\'"
               "[/\\\\]\\bazel-stonehenge\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories r)))

(use-package lsp-ui
  :commands lsp-ui
  :config
  (setq lsp-ui-doc-max-height 20))

;;;; Projectile

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode t))

;;;; Clojure

(use-package clojure-mode
  :hook ((clojure-mode . lsp-deferred)
         (clojure-mode . cljstyle-format-on-save-mode)))

(use-package cljstyle
  :after clojure-mode
  :straight (cljstyle
             :type git
             :host github
             :repo "cfclrk/cljstyle.el"))

(use-package cider
  :after clojure-mode
  :config
  (setq cider-save-file-on-load t))

(use-package monorepl
  :after cider
  :straight (monorepl
             :local-repo "~/Work/stonehenge"
             :files ("development/emacs/monorepl.el"))
  :config
  (setq monorepl-STONEHENG-PATH (expand-file-name "~/Work/stonehenge")))

;;;; yasnippet

;; LSP uses yasnippet to expand snippets. So enabling yas-global-mode is
;; necessary even if you don't load any snippets.

(use-package yasnippet
  :config (yas-global-mode 1))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((cljr-magic-require-namespaces
      ("io" . "clojure.java.io")
      ("set" . "clojure.set")
      ("str" . "clojure.string")
      ("walk" . "clojure.walk")
      ("zip" . "clojure.zip")
      ("d" . "datomic.api")
      ("log" . "taoensso.timbre")
      ("component" . "com.stuartsierra.component")
      ("http" . "clj-http.client")
      ("json" . "cheshire.core"))
     (cljr-suppress-no-project-warning . t)
     (cider-ns-refresh-after-fn . "system/override")
     (cider-ns-save-files-on-refresh . t)
     (cider-known-endpoints
      ("localhost" "7888")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
