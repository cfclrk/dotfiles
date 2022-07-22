;;; init-markdown.el --- Initialize markdown config  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;;; grip

;; `grip-mode' renders markdown files using GitHub's rendering API. That can be
;; very useful, but it
;;
;; 1. Requires an internet connection
;; 2. Can't be easily customized
;; 3. Doesn't render mermaid diagrams
;;
;; `grip-mode' can be a great way to double-check how something will look in
;; GitHub, but more commonly your day-to-day rendering should be done using
;; `markdown-live-preview-mode'.

(use-package grip-mode
  :after markdown-mode
  ;; TODO: bind C-c C-z to markdown-live-preview-switch-to-output
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode))
  :config
  (require 'auth-source)
  (let ((credential (auth-source-user-and-password
                     "api.github.com" "cclark-splash^forge")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential))))

;;;; markdownx

(use-package markdownx
  :straight (markdownx
             :type git
             :host github
             :repo "cfclrk/markdownx"
             :files (:defaults "resources"))
  :config
  (setq markdownx-github-theme "dark-dimmed"
        markdownx-mermaid-theme "dark"
        markdownx-code-block-theme "github-dark-dimmed"))

;;;; markdown

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . markdown-toc-mode))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map
              ("C-c C-z" . markdown-live-preview-switch-to-output))
  :init
  (setq markdown-split-window-direction 'right)
  :config
  (setq markdown-italic-underscore t
        markdown-list-indent-width 2
        markdown-enable-wiki-links t
        markdown-enable-math t)

  (setq whitespace-style '(face tabs empty trailing))

  ;; Restart whitespace mode so that is properly uses `whitespace-style'.
  (whitespace-mode -1)
  (whitespace-mode +1))

(use-package markdown-toc
  :straight (markdown-toc
             :host github
             :repo "ardumont/markdown-toc"
             :fork (:host github
                          :repo "cfclrk/markdown-toc"))
  :bind (:map markdown-toc-mode-map
              ("M-." . markdown-toc-follow-link))
  :config
  (setq
   ;; markdown-toc-start "<!--ts-->"
   ;; markdown-toc-end "<!--te-->"
   ;; markdown-toc-title nil
   ;; Do not include first h1
   markdown-toc-transform-fn 'cdr))

;; edit-inderect is required to use C-c ' (markdown-edit-code-block), which lets
;; you edit source blocks in another buffer (similar to org-edit-special)
(use-package edit-indirect)

(provide 'init-markdown)
;;; init-markdown.el ends here
