;;; init-markdown.el --- Initialize markdown config  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;;; markdown

(defun my/markdown-mode-hook ()
  "Init hook for markdown-mode."
  (setq fill-column 100
        visual-fill-column-center-text t))

(use-package markdown-mode
  :after visual-fill-column
  :hook ((markdown-mode . my/markdown-mode-hook)
         (markdown-mode . markdown-toc-mode)
         (markdown-mode . visual-line-mode)
         (markdown-mode . visual-fill-column-mode))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map
              ("C-c C-z" . markdown-live-preview-switch-to-output))
  :init
  (setq markdown-split-window-direction 'right)
  :custom
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

(provide 'init-markdown)
;;; init-markdown.el ends here
