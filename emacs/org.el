;;; org.el -- Org mode customization  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'org)
(require 'prelude-org)
(require 'ox-html)
(require 'ob-clojure)

(prelude-require-packages '(htmlize
                            org
                            org-bullets
                            ox-hugo))

;;; ----------------------------------------------------------------------------

;; I have to keep some configuration defined globally (instead of inside
;; my-org-mode-hook) because of how org-mode caches some settings.
;;
;; When these settings are in my-org-mode-hook, they can still work if you force
;; org-mode to be loaded twice on it's first load (using revert-buffer or
;; org-mode-restart).
;;
;; I'm not entirely clear on how org-mode caches these settings. For some more
;; info, see: https://emacs.stackexchange.com/a/30623/6769

;; Set initial visibility (depth)
(setq org-startup-folded 't)

;; todo
(setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "DONE")))
(setq org-enforce-todo-dependencies t)

;;; ----------------------------------------------------------------------------
;;; my-org-mode-hook
;;; ----------------------------------------------------------------------------

(defun my-org-mode-hook ()
  "Customize org mode."

  (setq org-src-window-setup 'split-window-below)

  ;; Use UTF8 bullets in Org mode headings
  (org-bullets-mode 1)

  ;; GitHub Flavored Markdown exporter
  (require 'ox-gfm)

  ;; babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (dot . t)
     (python . t)
     (shell . t)))
  (setq org-confirm-babel-evaluate nil)

  ;; ob-clojure
  (setq org-babel-clojure-backend 'cider)

  ;; exporting
  (setq org-html-doctype "html5"
        org-html-html5-fancy t
        org-export-with-sub-superscripts nil
        org-html-preamble nil
        org-html-postamble nil
        org-html-head nil)

  ;; publishing
  (require 'ox-publish)
  (setq org-publish-project-alist

        '(("Blog"
           :base-directory "~/Projects/jekyll/blog/org/posts/"
           :publishing-directory "~/Projects/jekyll/blog/_posts/"
           base-extension "org"
           :publishing-function org-html-publish-to-html
           :body-only t)

          ("orgtests"
           :base-directory "~/Luminal/orgtests/org/"
           :publishing-directory "~/Luminal/orgtests/dist/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :body-only t)))

  (defun cfc/wrap-with-vue-template ()
    (with-temp-buffer
      (insert-file-contents "~/cat.txt")
      (write-region
       (concat "<template><div>" (buffer-string) "</div></template>") nil "~/cat2.txt")))

  (defun cfc/on-every-src-block (fn)
    "Visit every SRC block and evaluate FN."
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward "^\s*#[+]BEGIN_SRC" nil t)
          (let ((element (org-element-at-point)))
            (when (eq (org-element-type element) 'src-block)
              (funcall fn element)))))
      (save-buffer)))

  (defun cfc/org-remove-results ()
    "Remove all RESULTS blocks in an org file."
    (interactive)
    (cfc/on-every-src-block 'org-babel-remove-result)))

(add-hook 'org-mode-hook 'my-org-mode-hook)
