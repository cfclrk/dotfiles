;;; projectile-discovery.el -- My project discovery config  -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is tangled from projectile_root.org

;;; Code:

(require 'projectile)
(require 'f)

(setq my-project-root-files
      (-concat '("go.mod")
               projectile-project-root-files-bottom-up
               projectile-project-root-files))

(defun any-file-exists? (files dir)
  "True if any of the filenames in FILES is in DIR.
FILES is a list of filenames. DIR is a path to a directory."
  (cl-some
   (lambda (filename) (f-exists? (f-expand filename dir)))
   files))

(defun my-project-root (dir &optional list)
  "Identify a project root.
Perform a bottom-up search for files in LIST starting from DIR.
Always return the lowest directory that has any file in LIST. If
LIST is nil, use `my-project-root-files' instead. Return the
first (bottommost) matched directory or nil."
  (let ((marker-files (or list my-project-root-files)))
    (f--traverse-upwards (any-file-exists? marker-files it)
                         dir)))

(setq projectile-project-root-functions
      '(projectile-root-local
        projectile-root-bottom-up
        my-project-root  ;;  Our new function
        projectile-root-top-down
        projectile-root-top-down-recurring))

(setq projectile-project-root-files-bottom-up
      '(".projectile"))

;;; projectile-discovery.el ends here
