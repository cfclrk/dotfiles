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

;;;; markdown

(defun markdown-live-preview-window-xwidget-webkit (file)
  "Preview FILE with xwidget-webkit.
To be used with `markdown-live-preview-window-function'."
  (let ((uri (format "file://%s" file)))
    (xwidget-webkit-browse-url uri)
    xwidget-webkit-last-session-buffer))

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . markdown-toc-mode))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map
              ("C-c C-z" . markdown-live-preview-switch-to-output))
  :init
  ;; TODO: Update/monkeypatch markdown-add-xhtml-header-and-footer. Use mustache
  ;; templates.
  (setq markdown-command "multimarkdown"
        markdown-list-indent-width 2
        markdown-spaces-after-code-fence 0
        markdown-fontify-code-blocks-natively t
        markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-make-gfm-checkboxes-buttons t
        markdown-content-type "application/xhtml+xml"
        markdown-split-window-direction 'right
        markdown-content-type "")
  (setq markdown-live-preview-window-function
        #'markdown-live-preview-window-xwidget-webkit)

  ;; A file I made using generate-github-markdown-css
  (setq my-markdown-css (expand-file-name
                         "github_dark_dimmed.css"
                         user-emacs-directory))

  (setq
   ;; Note, these styles are only applied to the div with a "markdown-body"
   ;; class attribute. markdown-mode does not automatically add a
   ;; `class="markdown-body"' anywhere, so we need to do that ourselves
   ;; somewhere (I do it in the header.html file below).
   ;;
   ;; Also, the URL below is some GitHub CSS, available on a CDN. But, it only
   ;; includes styles for the "dark" and "light" GitHub themes. The xwdidget
   ;; browser wil use one of those - whichever your system theme is. Using the
   ;; dark-dimmed theme requires some other solution. Also, I like having local
   ;; CSS so it doesn't have to be fetched over the internet.
   ;;
   ;; - "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.1.0/github-markdown.min.css"
   markdown-css-paths
   `(;; A file I made using generate-github-markdown-css
     ,my-markdown-css

     ;; For highlighting code blocks with highlightjs
     "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.1/styles/github-dark-dimmed.min.css")

   markdown-xhtml-header-content
   (f-read-text
    (file-truename
     (expand-file-name
      "markdown/header.html"
      user-emacs-directory))))
  :config
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
