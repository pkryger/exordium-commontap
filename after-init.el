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

;; A shorter list of packages
(when (string= exordium-melpa-package-repo exordium-pinned-melpa-package-repo)
  (cl-delete-if (lambda (elt)
                  (string= "melpa" (car elt)))
                package-archives))

;; ITERM2 MOUSE SUPPORT from https://www.emacswiki.org/emacs/iTerm2
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e)))

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

(use-package use-package-ensure-system-package
  :ensure t)

(use-package ispell
  :ensure-system-package aspell
  :config
  ;; spell checks as suggested by
  ;; http://blog.binchen.org/posts/effective-spell-check-in-emacs.html
  ;; http://blog.binchen.org/posts/how-to-spell-check-functionvariable-in-emacs.html
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args `("--sug-mode=ultra"
                           ,@(when exordium-osx '("--camel-case"))))
  (setq ispell-dictionary "british"))

(use-package flyspell
  :diminish
  :init
  (use-package flyspell-correct-helm
    :config
    (setq flyspell-correct-interface #'flyspell-correct-helm))

  (defun pk/flyspell-jump-and-correct-word (event)
    (interactive "e")
    (deactivate-mark)
    (mouse-set-point event)
    (redisplay)
    (flyspell-correct-word-before-point event (point)))

  :config
  (setq flyspell-issue-message-flag nil)

  :hook
  ((git-commit-mode . flyspell-mode)
   (org-mode        . flyspell-mode)
   (text-mode       . flyspell-mode))

  :bind (:map flyspell-mouse-map
         ([mouse-2] . nil)
         ([H-mouse-1] . pk/flyspell-jump-and-correct-word)
         :map flyspell-mode-map
         ("C-;" . flyspell-correct-wrapper)
         ([(control ?\,)] . nil)
         ([(control ?\.)] . nil)))

(set-time-zone-rule "/usr/share/zoneinfo/Europe/London")

(require 'cc-mode)
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
(require 'auto-complete)
(setq ac-auto-start nil     ;; original 0.3
      ac-auto-show-menu nil ;; original 0.8
      ac-delay 0.3)         ;; original 0.1

;; garbage collect magic hack from https://gitlab.com/koral/gcmh
(use-package gcmh
  :diminish
  :config
  (gcmh-mode 1))

;; Alternative approach to gcmh
;; (defun dotfiles--gc-on-last-frame-out-of-focus ()
;;   "GC if all frames are inactive."
;;    (if (seq-every-p #'null (mapcar #'frame-focus-state (frame-list)))
;;         (garbage-collect)))
;; (add-function :after after-focus-change-function
;;               #'dotfiles--gc-on-last-frame-out-of-focus)

(use-package deft
  :config
  (setq deft-extensions '("org" "txt" "md"))
  (setq deft-default-extension "org")
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 0))

(require 'ace-window)
;; Use M-o for ace-window
(global-set-key (kbd "M-o") #'ace-window)
;; Use bigger font for ace window
(custom-set-faces
 '(aw-leading-char-face
   ((t
     (:foreground "red" :bold t :height 2.5)))))
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;; Limit scope to a frame, as the neither global nor visible play nice with tabs
(setq aw-scope 'frame)

(use-package desktop
  :ensure nil
  :config
  ;; Don't save some buffers in desktop
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  (add-to-list 'desktop-modes-not-to-save 'helpful-mode)
  (add-to-list 'desktop-modes-not-to-save 'helm-major-mode)
  :custom
  (desktop-files-not-to-save
        (rx-let ((path (+ (or alnum digit "." "/" "-" "_" "~"))))
          (rx (or (seq string-start "/" (zero-or-more (not (any "/" ":"))) ":")
                  (seq "(ftp)" string-end)
                  (seq string-start path "/emacs/" path "/lisp/" path
                       ".el.gz" string-end)
                  (seq string-start path "/.emacs.d/elpa/" path
                       ".el" string-end)))))
  (desktop-restore-eager 8))

(use-package which-key
  :config
  (which-key-mode)
  (diminish 'which-key-mode))

(defcustom pk/python-bootstrap-packages
  '("epc" "jedi" "pytest")
  "List of packages to install as a part of `python-bootstrap'.
N.B. The `epc' and `jedi' are required for completion support.
     The `pytest' is optional but necessary for `python-pytest'."
  :type 'list)

(defcustom pk/python-bootstrap-requirements
  '("requirements.txt" "requirements-dev.txt")
  "List of requirements files to use as a part of `python-bootstrap'.
The first file found in a project will be used."
  :type 'list)

(use-package python
  :ensure nil
  :init
  (setq python-shell-interpreter "python3"))

(use-package py-autopep8
  ;; TODO: add if requested
  :ensure-system-package autopep8
  :ensure t
  :hook (python-mode . py-autopep8-enable-on-save))

(use-package python-pytest
  :ensure t
  :bind (:map python-mode-map
              ("C-c t" . python-pytest-dispatch))
  :init
  (setq python-pytest-executable
        (concat python-shell-interpreter " -m pytest")))

(use-package company-jedi
  :ensure t
  :hook
  (python-mode . jedi-mode) ;; needs `jedi' and `epc' to be available
  :config
  (add-to-list 'company-backends 'company-jedi)
  (setq jedi:server-command
        (list python-shell-interpreter jedi:server-script)))

;; See https://blog.adam-uhlir.me/python-virtual-environments-made-super-easy-with-direnv-307611c3a49a
;; for layout thing
(use-package direnv
  :ensure-system-package direnv
  :ensure t
  :config
  (direnv-mode))

(defun pk/python-bootstrap (dir)
  "In a given `DIR' bootstrap python environment.
Such a bootstrap will provide a dedicated virtual environment via `direnv'
python layout with:
- `jedi' completion support,
- `pytest' for running tests,
- install packages from `pk/python-bootstrap-packages',
- install packages from the first requirements file from
  `pk/python-bootstrap-requirements'."
  (interactive (list (read-directory-name
                      (concat "Boostrap python in directory: ")
                      (projectile-project-root))))
  (if-let ((python (when (string-match "python\[23\]" python-shell-interpreter)
                     (match-string 0 python-shell-interpreter))))
      (let* ((pip-command `(,python-shell-interpreter "-m" "pip" "install"))
             (requirements
              (when-let ((root (projectile-project-root))
                         (match
                          (seq-find
                           (lambda (elt)
                             (file-exists-p (f-join root elt)))
                           pk/python-bootstrap-requirements)))
                (f-join root match)))
             (project (or (projectile-project-name) (f-filename dir)))
             (progress 0)
             (reporter (make-progress-reporter
                        (format "Bootstrapping python direnv in %s " project)
                        progress (+ 2 ;; 1 for `.envrc' file, 2 for `direnv-allow'
                                    (length pk/python-bootstrap-packages)
                                    (if requirements 1 0)))))
        (when-let ((envrc-file (f-join dir ".envrc"))
                   (_continue
                    (or (not (file-exists-p envrc-file))
                        (y-or-n-p
                         (format "%s already exists.  Overwrite it before continuing? "
                                 envrc-file)))))
          (with-temp-file envrc-file
            (when-let ((srcdir (f-join dir "src"))
                       (_exists (file-directory-p srcdir)))
              (insert (concat "export PYTHONPATH=" srcdir ":${PYTHONPATH}\n")))
            (insert (concat "layout_" python "\n"))))
        (progress-reporter-update
         reporter (incf progress) "[allowing direnv...]")
        (direnv-allow)
        (when-let ((buf (get-buffer "*Shell Command Output*")))
          (with-current-buffer buf
            (erase-buffer)))
        (dolist (package pk/python-bootstrap-packages)
          (progress-reporter-update
           reporter (incf progress) (format "[installing %s...]" package))
          (let ((shell-command-dont-erase-buffer 'end-last-out))
            (shell-command (concat (s-join " " pip-command) " " package))))
        (when requirements
          (progress-reporter-update
           reporter (incf progress) (format "[installing from %s...]"
                                            (file-name-base requirements)))
          (lexical-let ((out-buffer (get-buffer-create
                                     (concat "*python-bootstrapper*<" project ">")))
                        (buffers (projectile-project-buffers)))
            (dolist (buf buffers)
              (with-current-buffer buf
                (setq mode-line-process
                      (propertize " [bootstrapping]"
                                  'help-echo (format "See progress in %s"
                                                     (buffer-name out-buffer))
                                  'font-lock-face 'magit-mode-line-process))))
            (with-current-buffer out-buffer
              (read-only-mode 0)
              (erase-buffer)
              (read-only-mode)
              (comint-mode))
            (make-process :name "pk/python-bootstrapper"
                          :buffer out-buffer
                          :command `(,@pip-command "-r" ,requirements)
                          :noquery t
                          :filter #'comint-output-filter
                          :sentinel #'(lambda (_process _event)
                                        (dolist (buf buffers)
                                          (when (buffer-live-p buf)
                                            (with-current-buffer buf
                                              (setq mode-line-process nil))))))))
        (progress-reporter-done reporter))
    (message "Cannot create .envrc for %s" python-shell-interpreter)))


(use-package helpful
  :ensure t
  :bind (:map global-map
              ;; Note that the built-in `describe-function' includes both functions
              ;; and macros. `helpful-function' is functions only, so we provide
              ;; `helpful-callable' as a drop-in replacement.
              ("C-h f" . #'helpful-callable)
              ;; Look up *F*unctions (excludes macros).
              ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
              ;; already links to the manual, if a function is referenced there.
              ("C-h F" . #'helpful-function)
              ("C-h v" . #'helpful-variable)
              ("C-h k" . #'helpful-key)
              ;; Look up *C*ommands.
              ;; By default, C-h C is bound to describe `describe-coding-system'. I
              ;; don't find this very useful, but it's frequently useful to only
              ;; look at interactive functions.
              ("C-h C" . #'helpful-command)
              ;; Lookup the current symbol at point. C-c C-d is a common keybinding
              ;; for this in lisp modes.
              :map emacs-lisp-mode-map
              ("C-c C-d" . #'helpful-at-point)))

(use-package page-break-lines
  :ensure t
  :config
  (add-to-list 'page-break-lines-modes 'helpful-mode)
  (global-page-break-lines-mode))

;; Use the same keys for helm-swoop-edit as in magit-commit and
;; helm-projectile-ag
(require 'helm-swoop)
(eval-after-load "helm-swoop"
  '(progn
     (define-key helm-swoop-edit-map (kbd "C-c C-c") 'helm-swoop--edit-complete)
     (define-key helm-swoop-edit-map (kbd "C-c C-k") 'helm-swoop--edit-cancel)
     (define-key helm-swoop-edit-map (kbd "C-c C-q C-k") 'helm-swoop--edit-delete-all-lines)))

;; Diminish some modes
(diminish 'eldoc-mode)
(diminish 'auto-revert-mode)
(diminish 'undo-tree-mode)

;;;###autoload
(define-transient-command pk/magit-smerge ()
  "Dispatch smerge command."
  :transient-suffix     'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  [["Movement"
    ("n" "next hunk" smerge-next)
    ("p" "prev hunk" smerge-prev)
    ("C-n" "next line" next-line)
    ("C-p" "prev line" previous-line)
    ("C-l" "recenter" recenter-top-bottom)]
   ["Merge action"
    ("b" "keep base" smerge-keep-base)
    ("u" "keep upper" smerge-keep-upper)
    ("l" "keep lower" smerge-keep-lower)
    ("a" "keep all"   smerge-keep-all)
    ("c" "keep current" smerge-keep-current)]
   ["Diff action"
    ("= <" "upper/base" smerge-diff-base-upper)
    ("= =" "upper/lower" smerge-diff-upper-lower)
    ("= >" "base/lower" smerge-diff-base-lower)
    ("R" "refine" smerge-refine)
    ("E" "ediff" smerge-ediff)] ;; TODO: this SIGSEGVs when the `pk/mac-auto-operator-composition-mode' is on
   ["Other"
    ;; TODO: add save and back to magit-status
    ("C" "combine with next" smerge-combine-with-next)
    ("r" "resolve" smerge-resolve)
    ("k" "kill current" smerge-kill-current)
    ("z" "undo" undo)]])

(add-hook 'magit-diff-visit-file-hook
          (lambda ()
            (when smerge-mode
              (define-key smerge-mode-map
                (kbd (concat smerge-command-prefix "t")) #'pk/magit-smerge)
              (pk/magit-smerge))))

(use-package swiper-helm
  :demand t
  :bind (
  ("C-s" . #'swiper-helm)
  ("C-r" . #'swiper-helm)))

;; Disable some ido hooks for helm mode
(when exordium-helm-everywhere
  (advice-remove 'call-interactively #'call-interactively@ido-cr+-record-current-command))

;; Use python3 for org
(setq org-babel-python-command "python3")
;; Don't ask for confirmation for code blocks (rather use :eval no)
(setq org-confirm-babel-evaluate nil)
;; Make tabs work nativly in org mode's src blocks
(setq org-src-tab-acts-natively t)

(use-package magit-todos
  :ensure-system-package (rg . ripgrep)
  :config
  (magit-todos-mode))

;; Load R as well
(use-package ess
  :config
  (require 'ess-site)
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '((R . t)))))

(use-package url
  :ensure nil
  :config
  (defun pk/url-netrc-auth (url &optional _prompt _overwrite _realm _args)
    "Get the token stored in `netrc' for the given URL."
    (let* ((href (if (stringp url)
		             (url-generic-parse-url url)
		           url))
	       (host (url-host href))
	       (user (or (url-user href) user-login-name))
           (secret (plist-get
                    (car (auth-source-search :host host
                                             :user user
                                             :type 'netrc
                                             :require '(:secret)
                                             :max 1))
                    :secret)))
      (when secret
        ;; TODO: Update handling of `gitlab': see `ghub--auth'
        ;; - Private-Token header
        ;; - no "token" prefix
        (concat "token "
                (encode-coding-string (if (functionp secret)
                                          (funcall secret)
                                        secret)
                                      'utf-8)))))
  (url-register-auth-scheme "netrc" #'pk/url-netrc-auth 1))

(use-package restclient)
(use-package restclient-helm)
(use-package ob-restclient
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '((restclient . t)))))

(use-package company-restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

(require 'map)
(when (fboundp 'mac-auto-operator-composition-shape-gstring)
  (defcustom pk/mac-auto-operator-composition-strings
    '(;; c++
      "!=" "%" "%=" "&" "&&" "&&=" "&=" "*" "**" "***" "*/" "*=" "++" "+="
      "--" "-=" "->" ".*" "..." "/" "/*" "/**" "//" "///" "/=" "::" "<" "<<"
      "<<<" "<<=" "<=" "<=>" "=" "==" ">" ">=" ">>" ">>=" ">>>" "?:" "\\\\"
      "\\\\\\" "^=" "|" "|=" "||" "||=" "~" "~=" "[]"
      ;; programming in non-c++ and nice stuff
      "__" "@" "@@" "!!" "===" "!==" "=>" "=~" ":=" "[:]"  "/>" "</>" "</" "<>"
      "<-" ";;" "\\n" "www" ".."
      ;; org-mode ballots -> they are unicode chars, not glyphs
      ;; "[ ]" "[X]" "[-]"
      ;; fira sans
      "fi" "fl"
      )
    "Sequence of strings used in automatic operator composition.
Customised for FiraCode font: https://github.com/tonsky/FiraCode"
    :type 'list)

  ;; ligatures, based on `mac-auto-operator-composition-mode'
  (define-minor-mode pk/mac-auto-operator-composition-mode
    "Toggle Mac Auto Operator Composition mode.
With a prefix argument ARG, enable Mac Auto Operator Composition
mode if ARG is positive, and disable it otherwise.  If called
from Lisp, enable the mode if ARG is omitted or nil.

Mac Auto Operator Composition mode automatically composes
consecutive occurrences of characters consisting of the elements
of `pk/mac-auto-operator-composition-strings' if the font
supports such a composition.  Some fonts provide ligatures for
several combinations of symbolic characters so such a combination
looks like a single unit of an operator symbol in a programming
language."
  :init-value nil
  :global t
  (if pk/mac-auto-operator-composition-mode
      (when (eq (terminal-live-p (frame-terminal)) 'mac)
        ;; Transform the `pk/mac-auto-operator-composition-strings' list into an alist.
        ;; The transformation is as follows:
        ;; - group all elements of the list by the first letter,
        ;; - each car of an alist element is first letter for a given group,
        ;; - each cdr of an alist element is list of substrings starting
        ;;   from the 1st position for each string in a given group.
        ;; i.e., ("ab" "a" "bc") -> ((?a "b" "") (?b "c"))
        (let (char-strings-alist)
          (mapc (lambda (string)
                  (push (if (< (length string) 1)
                            ""
                          (substring string 1))
                        (map-elt char-strings-alist (string-to-char string))))
                pk/mac-auto-operator-composition-strings)
          (mapc (lambda (char-strings)
                  (let ((new-rules `([,(concat "." (regexp-opt (cdr char-strings))) 0
                                      mac-auto-operator-composition-shape-gstring]))
                        (old-rules (aref composition-function-table (car char-strings))))
                    (set-char-table-range composition-function-table
                                          (car char-strings)
                                          (if (listp old-rules)
                                              (append old-rules new-rules)
                                            new-rules))))
                char-strings-alist))
        (set-char-table-range composition-function-table
                              ?0
                              '([".\\(?:x[a-fA-F0-9]\\)" 0
                                 mac-auto-operator-composition-shape-gstring]))
        (global-auto-composition-mode 1))
    (map-char-table
     (lambda (c rules)
       (when (consp rules)
         (let (new-rules removed-p)
           (dolist (rule rules)
             (if (eq (aref rule 2) 'mac-auto-operator-composition-shape-gstring)
                 (setq removed-p t)
               (push rule new-rules)))
           (if removed-p
               (set-char-table-range composition-function-table c
                                     (nreverse new-rules))))))
     composition-function-table)
    (clrhash mac-auto-operator-composition-cache)))
(pk/mac-auto-operator-composition-mode))

;; Org mode are not a real ligatures - use prettify symbols for it
(add-hook 'org-mode-hook
          (lambda ()
            (push '("[ ]" . "☐") prettify-symbols-alist)
            (push '("[X]" . "☑") prettify-symbols-alist)
            (push '("[-]" . "▣") prettify-symbols-alist)
            (prettify-symbols-mode)))

;; Helm and ediff are having issues with ligatures
(add-hook 'helm-major-mode-hook
          (lambda ()
            (setq auto-composition-mode nil)))
;; Disabling ligatures in ediff mode only removes them in the ediff buffer
;; itself (the small buffer underneath) and not the buffers you compare. Which
;; is probably a preferred solution.
(add-hook 'ediff-mode-hook
          (lambda ()
            (setq auto-composition-mode nil)))

(use-package mixed-pitch
  :when exordium-osx
  :init
  ;; TODO: move to mixed pitch mode
  (defcustom mixed-pitch-fixed-pitch-cursor 'box
    "TODO: If non-nil, function `mixed-pitch-mode' changes the cursor.
When disabled, switch back to what it was before.

See `cursor-type' for a list of acceptable types."
    :type 'symbol
    :group 'mixed-pitch)

  ;; TODO: move to exordium or to mixed-pitch-mode
  (defcustom pk/mixed-pitch--inhibit-modes
    '(yaml-mode
      nxml-mode)
    "TODO: List of major modes that the `mixed-pitch-mode' should not be activated.
This is useful, when certain modes are derived from modes that `mixed-pitch-mode'
is activated."
    :type 'list)

  ;; TODO: move to mixed-pitch-mode
  (defun mixed-pitch--set-cursor ()
    (when (get-buffer-window nil 'visible)
      (if (font-match-p
           (font-spec :name (face-attribute 'variable-pitch :family))
           (font-at (max
                     (min (point)
                          (- (point-max) 1))
                     (point-min))))
          (setq cursor-type mixed-pitch-variable-pitch-cursor)
        (setq cursor-type mixed-pitch-fixed-pitch-cursor))))

  ;; TODO: move to mixed pitch mode
  (defun pk/mixed-pitch---post-command-hook ()
    (if mixed-pitch-mode
        ;; TODO: only install this when variable and fixed cursors are different
        (add-hook 'post-command-hook #'mixed-pitch--set-cursor nil :local)
      (remove-hook 'post-command-hook #'mixed-pitch--set-cursor :local)))

  ;; TODO: move this to exordium
  (defun pk/mixed-pitch--enable-mode-maybe ()
    (unless (or (and (boundp 'mixed-pitch-mode)
                     mixed-pitch-mode)
                (memq major-mode pk/mixed-pitch--inhibit-modes)
                (seq-intersection minor-mode-list pk/mixed-pitch--inhibit-modes))
      (mixed-pitch-mode)))

  :config
  ;; TODO: move to exordium and make configurable
  (set-face-attribute 'variable-pitch nil
                      :family (caar exordium-preferred-variable-fonts)
                      :height (cdar exordium-preferred-variable-fonts)
                      :weight 'normal)
  ;; TODO: Using `mixed-pitch-set-height' has issues with zooming text
  ;; either with `text-scale-mode' nor with `default-text-scale-mode'.
  ;; (setq mixed-pitch-set-height t)
  ;; TODO: move this to mixed-pitch-mode
  (add-hook 'mixed-pitch-mode-hook #'pk/mixed-pitch---post-command-hook)

  ;; TODO: move this to exordium theme
  (defface exordium-org-strike-through '((t (:strike-through t)))
    "Face to be used as a strike through in `org-mode'.
The default definition of a face for `+' is `(:strike-through)'.
However this makes `org-mode' to pick up some random font,
and not the one set as a `variable-pitch'. Making this a proper
face seems to fix the issue.")
  ;; TODO: move this to exordium theme
  (custom-set-variables '(org-emphasis-alist
                          (append
                           (seq-filter (lambda (elt)
                                         (not (string= "+" (car elt))))
                                       org-emphasis-alist)
                           '(("+" exordium-org-strike-through (:strike-through t))))))

  ;; TODO: move this to exordium
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-todo)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-done)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'exordium-org-wait)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'exordium-org-work)

  :hook
  ;;TODO: move to exordium and make a configurable list
  ((text-mode . pk/mixed-pitch--enable-mode-maybe)
   ;; markdown-mode and org-mode inherit from text-mode
   ;; (markdown-mode . pk/mixed-pitch--enable-mode-maybe)
   ;; (org-mode . pk/mixed-pitch--enable-mode-maybe)
   (gfm-mode . pk/mixed-pitch--enable-mode-maybe)))

;; TODO: move to exordium and likely hide behind the
;; `exordium-use-variable-pitch' and `exordium-complete-mode' set to `:company'
(use-package company-box
  :when exordium-osx
  :diminish
  :hook (company-mode . company-box-mode))

(add-hook 'git-commit-mode-hook 'turn-on-auto-fill)
(require 'forge)
(add-hook 'forge-post-mode-hook #'(lambda () (set-fill-column 1000)))
(defun pk/forge-markdown-preview ()
  "Preview current buffer as a `markdown-mode' would do."
  (interactive)
  (let ((temp-file (make-temp-file (file-name-base buffer-file-name)
                                   nil ".md"
                                   (buffer-string))))
    (with-temp-buffer
      (insert-file-contents temp-file t)
      (markdown-preview))
    (delete-file temp-file)))
(define-key forge-post-mode-map (kbd "C-c p p") #'pk/forge-markdown-preview)

(add-to-list 'forge-owned-accounts '("pkryger" . (remote-name "pkryger")))
(add-to-list 'forge-owned-accounts '("emacs-exordium" . (remote-name "exordium")))


;; Configure tabs
(use-package tab-bar
  :ensure nil
  :config
  (custom-set-variables '(tab-bar-separator "⎞⎛")
                        '(tab-bar-tab-hints t)
                        '(tab-bar-select-tab-modifiers '(hyper))
                        '(tab-bar-show t t))
  :bind (:map global-map
              ("M-<tab>" . #'tab-next)
              ("M-S-<tab>" . #'tab-previous)
              ("H-t" . #'tab-new)))

;; evil likes to turn itself on, let's disable it so no surprises
(use-package evil
  :init
  (defun pk/disable-evil-mode ()
    (unless exordium-enable-evil-mode
      (when (or evil-mode evil-local-mode)
        (turn-off-evil-mode))))
  :hook
  ((evil-mode . pk/disable-evil-mode)
   (evil-local-mode . pk/disable-evil-mode)))

;; a couple statistical goodies
(defun pk/quartile--internal (sequence quartile &optional method)
  "Return a given QUARTILE of a sorted SEQUENCE.
The optional METHOD is the same as in `pk/quartile'."
  (let* ((length (length sequence))
         (offset (if (and (cl-oddp length)
                          (eq method :include))
                     1 0)))
    (cl-flet ((median (sequence)
                      (let* ((length (length sequence))
                             (mid (/ length 2)))
                        (if (cl-oddp length)
                            (seq-elt sequence mid)
                          (/ (+ (seq-elt sequence (1- mid))
                                (seq-elt sequence mid))
                             2.0)))))
        (cond
         ((and (< 1 length) (eq quartile 2))
          (median sequence))
         ((and (< 2 length) (eq quartile 1))
          (median (seq-subseq sequence
                              0
                              (+ (/ length 2)
                                 offset))))
         ((and (< 2 length) (eq quartile 3))
          (median (seq-subseq sequence
                              (- (1+ (/ length 2))
                                 offset))))))))

(defun pk/quartile (sequence quartile &optional method sorted)
  "Return a given QUARTILE for the specified SEQUENCE.

When the optional METHOD is nil or `:exclude', the value is returned according
to Method 1 from Wiki: `https://en.wikipedia.org/wiki/Quartile'.

The METHOD can be `:include' to use Method 2 instead.

When SORTED is t it indicates the sequence is already sorted.

Return nil unless one of:
- QUARTILE is one of 1, 2, or 3,
- SEQUENCE length is >=1 and QUARTILE is 2,
- SEQUENCE length is >=2 and QUARTILE is one of 1 or 3."
  (pk/quartile--internal (if sorted
                             sequence
                           (cl-sort sequence '<))
                         quartile method))

(defun pk/median (sequence &optional sorted)
  "Return a median for the specified SEQUENCE.
The optional SORTED is the same as in `pk/quartile'"
  (pk/quartile sequence 2 nil sorted))

(defun pk/five-nums (sequence &optional method sorted)
  "Return a list consisting of (min q1 med q3 max) for the specified SEQUENCE.
The optional METHOD and SORTED are the same as in `pk/quartile'.
When some values cannot be calculated they are set to nil."
  (if (and (not sorted) sequence)
      (setq sequence (cl-sort sequence '<)))
  (list (when sequence (seq-min sequence))
        (pk/quartile sequence 1 method t)
        (pk/median sequence t)
        (pk/quartile sequence 3 method t)
        (when sequence (seq-max sequence))))

(defun pk/five-nums-with-header (sequence &optional method sorted)
  "Return a result of `pk/five-nums' for the specified SEQUENCE with a header.
This is meant as a convenience function for `org-mode' code block to be used
with ':output table'.  The optional METHOD and SORTED are the same as in
`pk/quartile'."
  (list (list "min" "q1" "med" "q3" "max")
        'hline
        (pk/five-nums sequence method sorted)))

;;
