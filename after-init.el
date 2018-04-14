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

(setq ispell-dictionary "british")
(add-hook 'git-commit-mode-hook 'turn-on-auto-fill)
(add-hook 'git-commit-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(set-time-zone-rule "/usr/share/zoneinfo/Europe/London")

;; Alias for C-c r [
(define-key c-mode-base-map [(control <)] (function rtags-location-stack-back))
;; Alias for C-c r [
(define-key c-mode-base-map [(control >)] (function rtags-location-stack-forward))

;; update version control status (git) in mode line
(setq auto-revert-check-vc-info t)

;; use f8 to start/stop rtags
(global-set-key
 (kbd "<f8>")
 '(lambda (&optional ARG)
    (interactive "P")
    (if ARG (rtags-stop)
      (rtags-start)
      (unless rtags-diagnostics-process
        (rtags-diagnostics)))))

;; tune auto-complete
(setq ac-auto-start nil     ;; original 0.3
      ac-auto-show-menu nil ;; original 0.8
      ac-delay 0.3)         ;; original 0.1

;; Use large cache only for minibuffer, use small otherwise, as per bling:
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq gc-cons-threshold (* 1024 1024 10))
(add-hook 'minibuffer-setup-hook #'(lambda ()
                                     (setq gc-cons-threshold (* 1024 1024 100))))
(add-hook 'minibuffer-exit-hook #'(lambda ()
                                    (setq gc-cons-threshold (* 1024 1024 10))))

;; Allegedly this also helps
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)
