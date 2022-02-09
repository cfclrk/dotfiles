;;; projectile-discovery.el -- My project discovery config -*- lexical-binding: t; -*-

;;; Commentary:

;; Update projectile's project discovery behavior in monorepos to consider
;; subprojects as project roots, rather than considering the monorepo root as
;; the project root.

;; See: https://www.cfclrk.com/articles/projectile_root.html

;; This file is tangled from
;; https://github.com/cfclrk/cfclrk.github.io/blob/main/articles/org/projectile_root.org

;;; Code:

(require 'projectile)
(require 'f)

(setq
 my-project-root-files
 (-concat
  projectile-project-root-files-bottom-up
  projectile-project-root-files))

;; Python
(add-to-list
 'my-project-root-files
 "requirements.txt")

;; Go
(add-to-list
 'my-project-root-files
 "go.mod")

(defun any-file-exists? (files dir)
  "True if any of FILES exist in DIR.
FILES is a list of file names and/or predicates.

An element of FILES can also be a predicate taking one
argument (a directory) and returning a non-nil value if that
directory is the one we're looking for.

DIR is a path to a directory."
  (cl-some
   (lambda (name)
     (if (stringp name)
         (f-exists? (f-expand name dir))
       (funcall name dir)))
   files))

(defun projectile-root-bottom-up-unprioritized (dir &optional list)
  "Identify a project root.
Perform a bottom-up search for files in LIST starting from DIR.
Always return the lowest directory that has any file in LIST. If
LIST is nil, use `my-project-root-files' instead. Return the
first (bottommost) matched directory or nil."
  (let ((marker-files (or list my-project-root-files)))
    (f--traverse-upwards
     (any-file-exists? marker-files it) dir)))

(setq projectile-project-root-functions
      '(projectile-root-local
        projectile-root-bottom-up
        projectile-root-bottom-up-unprioritized  ;;  Our new function
        projectile-root-top-down
        projectile-root-top-down-recurring))

(setq projectile-project-root-files-bottom-up
      '(".projectile"))

;;; projectile-discovery.el ends here
