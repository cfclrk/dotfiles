;;; cfclrk_org.el -- Org mode customization  -*- lexical-binding: t; -*-

;;; Commentary:

;; My customization for org-mode.

;;; Code:

(defun cfc/org-mode-hook ()
  "Customize `org-mode'."
  
  ;; Babel languages to load
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (dot . t)
     (emacs-lisp . t)
     (plantuml . t)
     (python . t)
     (shell . t)))
  )

(add-hook 'org-mode-hook 'cfc/org-mode-hook)

(provide 'cfclrk-org)
;;; cfclrk_org.el ends here
