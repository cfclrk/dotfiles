;;; clean-global-map.el -- Clean up the global-map  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; duplicates "M-g g" for `goto-line'
(keymap-unset global-map "M-g M-g" 'remove)

;; duplicates "M-g n" for `next-error'
(keymap-unset global-map "M-g M-n" 'remove)

;; duplicates "M-g p" for `previous-error'
(keymap-unset global-map "M-g M-p" 'remove)

;;; clean-global-map.el ends here
(provide 'clean-global-map)
