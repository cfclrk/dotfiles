(defun my-smartparens-config ()
  ;; Activates show-smartparens-mode. Turn on visualization of matching pairs.
  (show-smartparens-global-mode t)

  ;; Create a key prefix. I like having a prefix so that which-key can show me
  ;; all the usual actions I perform.
  (global-unset-key (kbd "M-c"))  ; Was capitalize-word.
  (define-prefix-command 'sp-prefix-key-map)
  (define-key smartparens-mode-map (kbd "M-c") sp-prefix-key-map)

  ;; Slurping and barfing with Shift
  (define-key smartparens-mode-map (kbd "S-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "S-<left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-S-<right>") 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-S-<left>") 'sp-backward-slurp-sexp)

  ;; Copy
  (define-key sp-prefix-key-map (kbd "c") 'sp-copy-sexp)

  ;; Splicing
  (define-prefix-command 'sp-splice-key-map)
  (define-key sp-prefix-key-map (kbd "s") sp-splice-key-map)
  (define-key sp-splice-key-map (kbd "s") 'sp-splice-sexp)
  (define-key sp-splice-key-map (kbd "f") 'sp-splice-sexp-killing-forward)
  (define-key sp-splice-key-map (kbd "b") 'sp-splice-sexp-killing-backward)
  (define-key sp-splice-key-map (kbd "a") 'sp-splice-sexp-killing-around)

  ;; Wrapping
  (define-prefix-command 'sp-wrap-key-map)
  (define-key sp-prefix-key-map (kbd "r") sp-wrap-key-map)
  (define-key sp-wrap-key-map (kbd "r") 'sp-wrap-round)  ; ()
  (define-key sp-wrap-key-map (kbd "s") 'sp-wrap-square) ; []
  (define-key sp-wrap-key-map (kbd "c") 'sp-wrap-curly)  ; {}
  (define-key sp-wrap-key-map (kbd "u") 'sp-unwrap-sexp)
  (define-key sp-wrap-key-map (kbd "w") 'sp-rewrap-sexp)

  ;; Selection
  (define-key sp-prefix-key-map (kbd "n") 'sp-select-next-thing)
  (define-key sp-prefix-key-map (kbd "p") 'sp-select-previous-thing-exchange))
