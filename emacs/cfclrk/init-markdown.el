;;; init-markdown.el --- Initialize markdown config  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun my/markdown-mode-hook ()
  "Init hook for markdown-mode."
  (message "Running my/markdown-mode-hook, setting fill-column to 90")
  (setq fill-column 90
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
  :bind ((:map markdown-mode-map
               ("C-c C-z" . markdown-live-preview-switch-to-output))
         (:map markdown-mode-command-map
               ("v" . markdown-view-mode)))
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
  :ensure (markdown-xwidget
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

(use-package markdown-toc
  :after markdown-mode
  :ensure (markdown-toc
           :host github
           :repo "cfclrk/markdown-toc"
           :depth nil))

(provide 'init-markdown)
;;; init-markdown.el ends here
