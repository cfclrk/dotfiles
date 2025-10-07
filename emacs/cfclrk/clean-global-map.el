;;; clean-global-map.el -- Clean up the global-map  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The global map is so full of keybindings for stuff I never use, and has many
;; duplicates. Here I start trying to clean it up a bit to free up keybindings.

;;; Code:

;; Unset `transpose-chars'. I use C-t as a prefix for testing functions.
(keymap-unset global-map "C-t" 'remove)

;; Duplicates "M-g g" for `goto-line'
(keymap-unset global-map "M-g M-g" 'remove)

;; Duplicates "M-g n" for `next-error'
(keymap-unset global-map "M-g M-n" 'remove)

;; Duplicates "M-g p" for `previous-error'
(keymap-unset global-map "M-g M-p" 'remove)

;; Run's `compose-mail'. I don't use Emacs for email.
(keymap-unset global-map "C-x m" 'remove)

;; I don't need a sepecial binding for Ibuffer
(keymap-unset global-map "C-x C-b" 'remove)

;;; clean-global-map.el ends here
(provide 'clean-global-map)
