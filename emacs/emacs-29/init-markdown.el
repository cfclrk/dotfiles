;;; init-markdown.el --- Initialize markdown config  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun my/markdown-mode-hook ()
  "Init hook for markdown-mode."
  (message "Running my/markdown-mode-hook, setting fill-column to 83")
  (setq fill-column 85
        visual-fill-column-center-text t))

(use-package markdown-mode
  :after visual-fill-column
  :hook ((markdown-mode . my/markdown-mode-hook)
         (gfm-mode . my/markdown-mode-hook)
         (markdown-mode . visual-line-mode)
         (markdown-mode . visual-fill-column-mode))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map
              ("C-c C-z" . markdown-live-preview-switch-to-output))
  :custom
  (markdown-split-window-direction 'right)
  (markdown-spaces-after-code-fence 0)
  (markdown-italic-underscore t)
  (markdown-list-indent-width 2)
  (markdown-enable-wiki-links t)
  (markdown-enable-math t)
  :config
  (setq whitespace-style '(face tabs empty trailing))
  ;; Restart whitespace mode so that is properly uses `whitespace-style'.
  (whitespace-mode -1)
  (whitespace-mode +1))

(use-package markdown-xwidget
  :after markdown-mode
  :elpaca (markdown-xwidget
           :host github
           :repo "cfclrk/markdown-xwidget"
           :files (:defaults "resources")
           :depth nil)
  :bind (:map markdown-mode-command-map
              ("x" . markdown-xwidget-preview-mode))
  :custom
  (markdown-xwidget-command "pandoc")
  (markdown-xwidget-github-theme "light-high-contrast")
  (markdown-xwidget-mermaid-theme "default")
  (markdown-xwidget-code-block-theme "default"))

;; edit-inderect is required to use C-c ' (markdown-edit-code-block), which lets
;; you edit source blocks in another buffer (similar to org-edit-special)
(use-package edit-indirect)

(provide 'init-markdown)
;;; init-markdown.el ends here
