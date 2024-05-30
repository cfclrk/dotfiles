;;; init-functions.el -- My emacs functions  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(message "Declaring my functions")

(defun upsert-alist (quoted-alist entry)
  "Insert or update ENTRY in QUOTED-ALIST.

First, if ENTRY is in QUOTED-ALIST, delete it. Then insert ENTRY.
This prevents duplicates of ENTRY in the alist. Example:

  (setq my-alist '((:foo . 1)))

  (upsert-alist 'my-alist '(:bar . 1))
  ;; => ((:bar . 1) (:foo . 1))

  (upsert-alist 'my-alist '(:foo . 2))
  ;; => ((:foo . 2) (:bar . 1))
"
  (let ((entry-key (car entry))
        (orig-alist (symbol-value quoted-alist)))
    (set quoted-alist
         (cons entry (assoc-delete-all entry-key orig-alist)))))

(defun my/lsp-remove-all-workspaces ()
  "Clear all LSP workspaces. Sometimes this fixes things."
  (interactive)
  (mapc
   'lsp-workspace-folders-remove
   (lsp-session-folders (lsp-session))))

(defun my/show-buffer-file-name ()
  "Display and copy the full path to the current file.
Adapted from Emacs Redux (emacsredux.com)."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (message filename)
    (kill-new filename)))

(defun my/set-font-size (font-size)
  "Set font height to the given FONT-SIZE.
This updates font size without changing the Emacs frame (i.e.
window) size.
- TODO: display current font size in prompt. You can
get it with: (face-attribute 'default :height).
- TODO: Bind to M-F1/M-F2"
  (interactive "nFont Size: ")
  (let ((frame-inhibit-implied-resize t))
    (set-face-attribute 'default nil :height font-size)
    (set-face-attribute 'mode-line nil :height font-size)))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times. Unlike
`backward-kill-word', do not add the word to the `kill-ring'.
See: https://stackoverflow.com/questions/6133799"
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun pprint (form &optional printcharfun)
  "Return a pretty-printed version of FORM.

Optional PRINTCHARFUN is as defined by `princ'."
  (princ (with-temp-buffer
           (cl-prettyprint form)
           (buffer-string))
         printcharfun))

(defun my/font-installed-p (font-name)
  "Check if font with FONT-NAME is available.
FONT-NAME is a string like 'Roboto Mono'."
  (find-font (font-spec :name font-name)))

(provide 'init-functions)
;;; init-functions.el ends here
