;;; clean-global-map.el -- Clean up the global-map  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Duplicates "M-g g" for `goto-line'
(keymap-unset global-map "M-g M-g" 'remove)

;; Duplicates "M-g n" for `next-error'
(keymap-unset global-map "M-g M-n" 'remove)

;; Duplicates "M-g p" for `previous-error'
(keymap-unset global-map "M-g M-p" 'remove)

;; Run's `compose-mail'. I don't use Emacs for email.
(keymap-unset global-map "C-x m" 'remove)

;;; clean-global-map.el ends here
(provide 'clean-global-map)
