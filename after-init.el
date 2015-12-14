(global-set-key
 (kbd "<f5>")
 (lambda (&optional force-reverting)
   "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
   (interactive "P")
   ;;(message "force-reverting value is %s" force-reverting)
   (if (or force-reverting (not (buffer-modified-p)))
       (revert-buffer :ignore-auto :noconfirm)
     (error "The buffer has been modified"))))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

(global-set-key
 (kbd "<C-f5>")
 'revert-all-buffers)

;; helm goodies
(require 'helm-ag)
(setq projectile-switch-project-action 'helm-projectile)
(substitute-key-definition
 'projectile-switch-project 'helm-projectile-switch-project
 projectile-mode-map)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)

(global-set-key (kbd "C-S-p") 'helm-ag)
(global-set-key (kbd "C-S-r") 'helm-ag-project-root)
(global-set-key (kbd "C-S-d") 'helm-do-ag)
(global-set-key (kbd "C-S-f") 'helm-do-ag-this-file)
(global-set-key (kbd "C-S-a") 'helm-projectile-ag)

(setq helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-ag-fuzzy-match t
      helm-projectile-fuzzy-match t
      helm-ff-fuzzy-matching t
      helm-ag-insert-at-point 'symbol
      helm-buffer-details-flag nil)

(setq ispell-dictionary "british")
(setq git-commit-mode-hook '(turn-on-auto-fill flyspell-mode))
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(set-time-zone-rule "/usr/share/zoneinfo/Europe/London")
