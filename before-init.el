;;; before-init.el --- Exordium before init -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(when-let* ((file (or load-file-name
                      buffer-file-name))
            (dir (file-name-directory file))
            (init-vc-checkout (expand-file-name "init-vc-checkout.el" dir))
            ((file-exists-p init-vc-checkout)))
  (with-eval-after-load 'init-force-elpa
    (load init-vc-checkout)))

(provide 'before-init)

;;; before-init.el ends here
