(defun splash-tinker ()
  "Start tinker for Website."
  (interactive)
  (let ((default-directory splash-website-dir))
      (make-comint "tinker"
                   "docker" nil
                   "exec" "-it" "app" "php" "artisan" "tinker"))
  (switch-to-buffer "*tinker*"))
