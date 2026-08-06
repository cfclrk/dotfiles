(defun splash-tinker ()
  "Start tinker for Website."
  (interactive)
  (let ((default-directory splash-website-dir))
    (make-comint "tinker"
                 "docker" nil
                 "exec" "-it" "app" "php" "artisan" "tinker"))
  (switch-to-buffer "*tinker*"))

(defun splash-bookmarks (site)
  (interactive (list
                (completing-read "Choose site: " (mapcar 'car splash-site-urls))))
  (let ((url (cdr (assoc site splash-site-urls))))
    (browse-url url)))

(global-set-key (kbd "C-c s") #'splash-bookmarks)
