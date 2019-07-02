;; emacs mac ports customisations, per
;; https://github.com/railwaycat/homebrew-emacsmacport
;; Keybonds
(when exordium-osx
  (global-set-key [(hyper a)] 'mark-whole-buffer)
  (global-set-key [(hyper v)] 'yank)
  (global-set-key [(hyper c)] 'kill-ring-save)
  (global-set-key [(hyper s)] 'save-buffer)
  (global-set-key [(hyper l)] 'goto-line)
  (global-set-key [(hyper w)]
                  (lambda () (interactive) (delete-window)))
  (global-set-key [(hyper z)] 'undo)
  (global-set-key [(hyper q)] 'save-buffers-kill-emacs)

  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier 'meta)
  (setq mac-command-modifier 'hyper))

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

;; spell checks as suggested by
;; http://blog.binchen.org/posts/effective-spell-check-in-emacs.html
;; http://blog.binchen.org/posts/how-to-spell-check-functionvariable-in-emacs.html
(setq flyspell-issue-message-flag nil)
(when (executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args (list
        "--sug-mode=bad-spellers"
        "--run-together")))
        ;; "--run-together-limit=5"
        ;; "--run-together-min=2"
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
;;(setq auto-revert-check-vc-info t)

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

;; Don't auto save files in deft
(require 'deft)
(setq deft-extensions '("org"))
(setq deft-default-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-auto-save-interval 0)


;; Use M-o for ace-window
(global-set-key (kbd "M-o") #'ace-window)
;; Use bigger font for ace window
(custom-set-faces
 '(aw-leading-char-face
   ((t
     (:foreground "red" :bold t :height 2.5)))))
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; save state on exit
(desktop-save-mode 1)

(require 'which-key)
(which-key-mode)
(diminish 'which-key-mode)

;; Use python3 by default
(setq python-shell-interpreter "python3")

;; Apply PEP8 to python files
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; Turn anaconda on, this required at least jedi and factory_service python modules
(require 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(setq anaconda-mode-lighter nil)

;; Use anaconda for company
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Turn on page-break-lines-mode
(add-to-list 'page-break-lines-modes 'helpful-mode)
(global-page-break-lines-mode)

;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)

;; Look up *C*ommands.
;;
;; By default, C-h C is bound to describe `describe-coding-system'. I
;; don't find this very useful, but it's frequently useful to only
;; look at interactive functions.
(global-set-key (kbd "C-h C") #'helpful-command)

;; Use the same keys for helm-swoop-edit as in magit-commit and
;; helm-projectile-ag
(setq helm-swoop-edit-map
      (let (($map (make-sparse-keymap)))
        (define-key $map (kbd "C-c C-c") 'helm-swoop--edit-complete)
        (define-key $map (kbd "C-c C-k") 'helm-swoop--edit-cancel)
        (define-key $map (kbd "C-c C-q C-k") 'helm-swoop--edit-delete-all-lines)
        $map))

;; Diminish some modes
(diminish 'eldoc-mode)
(diminish 'auto-revert-mode)
(diminish 'flyspell-mode)

;; from https://github.com/alphapapa/unpackaged.el#hydra
(require 'hydra)
(require 'magit)
(defhydra smerge-hydra
  (:color pink :hint nil :post (smerge-auto-leave))
  "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("\C-m" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("R" smerge-refine)
  ("E" smerge-ediff)
  ("C" smerge-combine-with-next)
  ("r" smerge-resolve)
  ("k" smerge-kill-current)
  ("ZZ" (lambda ()
          (interactive)
          (save-buffer)
          (bury-buffer))
   "Save and bury buffer" :color blue)
  ("q" nil "cancel" :color blue))

(add-hook 'magit-diff-visit-file-hook (lambda ()
                                        (when smerge-mode
                                          (smerge-hydra/body))))

;; Use swiper - the swiper-helm seems to be less usable
(global-set-key (kbd "C-s") #'swiper)
(global-set-key (kbd "C-r") #'swiper)

;; Disable some ido hooks for helm mode
(when exordium-helm-everywhere
  (advice-remove 'call-interactively #'call-interactively@ido-cr+-record-current-command))
