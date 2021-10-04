;;; projectile-discovery.el -- My project discovery config  -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is tangled from projectile_root.org

;;; Code:

(require 'projectile)
(require 'dash)
(require 'f)

(setq my-project-root-files
      (-concat '("go.mod")
               projectile-project-root-files-bottom-up
               projectile-project-root-files))

(defun any-file-in-dir? (file-list dir)
  "True if any of the files in FILE-LIST is in the directory DIR.
Otherwise false."
  (--any (f-exists? (f-expand it dir)) file-list))

(defun my-project-root (dir &optional list)
  "Identify a project root in DIR by bottom-up search for files in LIST.
If LIST is nil, use `my-project-root-files' instead.
Return the first (bottommost) matched directory or nil if not found."
  (let ((marker-files (or list my-project-root-files)))
    (f--traverse-upwards
     (any-file-in-dir? marker-files it)
     dir)))

(setq projectile-project-root-functions
      '(projectile-root-local
        my-project-root  ;;  Insert our new function
        projectile-root-bottom-up
        projectile-root-top-down
        projectile-root-top-down-recurring))

;;; projectile-discovery.el ends here
