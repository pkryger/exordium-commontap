;; -*- lexical-binding: t; -*-
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
  (setq mac-command-modifier 'hyper)
  (setq mac-frame-tabbing nil))

(defun pk/iterm-cut-base64 (text)
  "Take TEXT and send it to iTerm2 to copy."
  (interactive)
  (let ((base-64 (base64-encode-string text :no-line-break)))
    (send-string-to-terminal (concat "\e]1337;Copy=:" base-64 "\a"))))
(defconst pk/interprogram-cut-function interprogram-cut-function
  "Save the default value of `interprogram-cut-function'.
This will be used in be used in `pk/dispatch-cut-function'")
(defun pk/dispatch-cut-function (text)
  "Dispatch the TEXT to the appropriate `interprogram-cut-function'."
  (if (display-graphic-p)
      (funcall pk/interprogram-cut-function text)
    (pk/iterm-cut-base64 text)))
(setq interprogram-cut-function #'pk/dispatch-cut-function)

;; https://ylluminarious.github.io/2019/05/23/how-to-fix-the-emacs-mac-port-for-multi-tty-access/
(use-package mac-pseudo-daemon
  :hook
  (after-init . mac-pseudo-daemon-mode)
  (after-init . server-start))

(setq disabled-command-function nil)
(setq confirm-kill-emacs 'y-or-n-p)

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

(use-package helm
  :diminish
  :bind
  (:map helm-command-map
        ("g" . #'helm-google-suggest)))

(use-package use-package-ensure-system-package)
(use-package quelpa-use-package)


(use-package ispell
  :ensure-system-package aspell
  :custom
  ;; spell checks as suggested by
  ;; http://blog.binchen.org/posts/effective-spell-check-in-emacs.html
  ;; http://blog.binchen.org/posts/how-to-spell-check-functionvariable-in-emacs.html
  (ispell-program-name "aspell")
  (ispell-extra-args `("--sug-mode=ultra"
                            "--run-together"
                            "--run-together-limit=6"
                           ,@(when exordium-osx '("--camel-case"))))
  (ispell-dictionary "british"))

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


(defun company-assignees (command &optional arg &rest ignored)
  "A `company-mode' backend for assigneees in `forge-mode' repository."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'pk/company-forge-assignees-backend))
    (prefix (when (and (or (and (boundp 'git-commit-mode)
                                git-commit-mode)
                           (derived-mode-p 'forge-post-mode))
                       (forge-get-repository 'full)
                       (looking-back
                        "@\\([a-z0-9]\\(?:[a-z0-9]\\|[-/]\\(?:[a-z0-9]\\)\\)\\{0,38\\}\\)?"
                        (max (- (point) 40)
                             (point-at-bol))))
              ;; IDK how to match end of a 'symbol' that is equal to an "@" or
              ;; is equal to an "@foo" in neither `git-commit-mode' nor
              ;; `forge-post-mode'. Hence it's handled manually.  The
              ;; `looking-back' above matches an "@" or an "@foo". When it was
              ;; the latter there was a match of the group 1. Store it before
              ;; next search.
              (let ((match (match-string 1)))
                ;; Check, if this is at the very end of the "@" or "@foo".  The
                ;; "@<point>@" also matches. Probably a few other characters,
                ;; substituting the second "@" in latter pattern, would also
                ;; give a positive result. Yet the `match' is "", so that's all
                ;; fine - all candidates will be shown.
                (when (or (looking-at "\\W")
                          (= (point) (point-max)))
                  (cons (or match "") t)))))
    (candidates (when-let ((repo (forge-get-repository 'full)))
                  (cl-remove-if-not
                   (lambda (assignee)
                     (string-prefix-p arg assignee))
                   (mapcar (lambda (assignee)
                             (propertize (cadr assignee)
                                         'full-name (caddr assignee)))
                           (oref repo assignees)))))
    (annotation (format " [%s]" (get-text-property 0 'full-name arg)))))

(add-to-list 'company-backends 'company-assignees)

(defun pk/forge-cleanup-known-repositories ()
  "Cleanup known repositories, that is repositories that worktree does not exist anymore."
  (interactive)
  (let ((to-delete (cl-remove-if
                    (lambda (repo)
                      (file-exists-p (nth 3 repo)))
                    (forge-sql [:select [githost owner name worktree] :from repository
                                        :order-by [(asc owner) (asc name)]]
                               [githost owner name worktree]))))
    (when (and to-delete
               (yes-or-no-p (format
                             "Do you really want to remove %s repositories from the db? "
                             (length to-delete))))
       (dolist (repo to-delete)
         (when-let ((host (nth 0 repo))
                    (owner (nth 1 repo))
                    (name (nth 2 repo))
                    (repo (forge-get-repository (list host owner name))))
           (message "Deleting %s/%s @%s..." owner name host)
           (let ((t0 (current-time))
                 (emacsql-global-timeout 120))
             (closql-delete repo)
             (message "- deletion of %s/%s @%s took %.06f"
                      owner name host (float-time (time-since t0))))))
         (magit-refresh))))

(defun pk/forge-cleanup-known-repositories-async ()
  "Cleanup known repositories, that is repositories that worktree does not exist anymore."
  (interactive)
  (let ((to-delete (cl-remove-if
                    (lambda (repo)
                      (file-exists-p (nth 3 repo)))
                    (forge-sql [:select [githost owner name worktree] :from repository
                                        :order-by [(asc owner) (asc name)]]
                               [githost owner name worktree]))))
    (when (and to-delete
               (yes-or-no-p (format
                             "Do you really want to remove %s repositories from the db? "
                             (length to-delete))))
      (async-start
       (lambda ()
         (package-initialize) ;; TODO: this doesn't seem to work
         (require 'forge)
         (let (results)
           (dolist (repo to-delete)
             (when-let ((host (nth 0 repo))
                        (owner (nth 1 repo))
                        (name (nth 2 repo))
                        (repo (forge-get-repository (list host owner name))))
               (let ((t0 (current-time))
                     (emacsql-global-timeout 120))
                 (closql-delete repo)
                 (setq results
                       (append
                        results
                        (list owner name host (float-time (time-since t0))))))))
           (magit-refresh)
           results))
       (lambda (results)
         ;;TODO: dosomething
         )))))


(use-package jenkinsfile-mode)
(use-package groovy-mode
  :after (yasnippet projectile)
  :init
  (defun pk/groovy-mode--create-test-files ()
    (setq-local projectile-create-missing-test-files t))
  :hook
  (groovy-mode . yas-minor-mode)
  (groovy-mode . pk/groovy-mode--create-test-files)
  :config
  (projectile-register-project-type 'pbnj '("pom.xml" "pbnj_lib_config.yaml")
                                    :project-file "pom.xml"
                                    :compile "mvn clean install"
                                    :test "mvn verify"
                                    :test-suffix "Test"
                                    :src-dir "src/"
                                    :test-dir "test/")
  (add-to-list 'yas-snippet-dirs
               (concat (file-name-directory (or load-file-name
                                                buffer-file-name)) "snippets")))

(use-package yaml-mode)
(use-package flycheck
  :custom
  (flycheck-global-modes '(not c++-mode c-mode org-mode))
  :config
  (flycheck-add-mode 'groovy 'jenkinsfile-mode)
  :hook
  (after-init . global-flycheck-mode))

(require 'cc-mode)
;; Alias for C-c r [
(define-key c-mode-base-map [(control <)] (function rtags-location-stack-back))
;; Alias for C-c r [
(define-key c-mode-base-map [(control >)] (function rtags-location-stack-forward))

;; update version control status (git) in mode line
;;(setq auto-revert-check-vc-info t)


(defconst pk/gc-cons-threshold (* 16 1024 1024))
;; From DOOM FAQ:
;; https://github.com/hlissner/doom-emacs/blob/64922dd/docs/faq.org#L215-L225
(defun pk/defer-garbage-collection ()
  "Use max value for gc, when in minibuffer.
It's so it won't slow expensive commands and completion frameworks."
  (setf gc-cons-threshold most-positive-fixnum))

(defun pk/restore-garbage-collection ()
  "Get back to the original gc threshold.
Defer it so that commands launched immediately after will enjoy the benefits."
  (run-at-time
   1 nil (lambda () (setf gc-cons-threshold pk/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'pk/defer-garbage-collection)
(add-hook 'minibuffer-exit-hook #'pk/restore-garbage-collection)

;; garbage collect magic hack from https://gitlab.com/koral/gcmh
;; tuned like in DOOM:
;; https://github.com/hlissner/doom-emacs/blob/db16e5c/core/core.el#L350-L351
;; https://github.com/hlissner/doom-emacs/blob/ed1996e/modules/lang/org/config.el#L548
(use-package gcmh
  :diminish
  :custom
  (gcmh-idle-delay 5)
  (gcmh-high-cons-threshold pk/gc-cons-threshold)
  :hook
  ;; TODO: this also needs to be after a buffer switch, including current value of `gc-cons-threshold'
  ;; likely in `buffer-list-update-hook'
  (org-mode . (lambda ()
                (setq-local gcmh-high-cons-threshold (* 2 pk/gc-cons-threshold))))
  (after-init . gcmh-mode))


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
     (:foreground "red" :bold t :height 1.5)))))
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;; Limit scope to a frame, as the neither global nor visible play nice with tabs
(setq aw-scope 'frame)


(defconst pk/desktop-files-not-to-save
  (if (version< "27" emacs-version)
      (rx-let ((path (+ (or alnum digit "." "/" "-" "_" "~"))))
        (rx (or (seq string-start "/" (zero-or-more (not (any "/" ":"))) ":")
                (seq "(ftp)" string-end)
                (seq string-start path "/emacs/" path "/lisp/" path
                     ".el.gz" string-end)
                (seq string-start path "/.emacs.d/elpa/" path
                     ".el" string-end))))
    "\\(\\`/[^/:]*:\\|(ftp)\\'\\)"))

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
  (desktop-files-not-to-save pk/desktop-files-not-to-save)
  (desktop-restore-eager 8))


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
  :custom
  (python-shell-interpreter  "python3")
  :hook
  (python-mode . (lambda ()
                   (setq fill-column 100))))

(use-package py-autopep8
  ;; TODO: add if requested
  :ensure-system-package autopep8
  :ensure t
  :custom
  (py-autopep8-options  '("--max-line-length" "100"))
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


;; Diminish some modes
(diminish 'eldoc-mode)
(diminish 'auto-revert-mode)
(diminish 'undo-tree-mode)


(use-package swiper
  :init
  (defun pk/swiper-C-r (&optional arg)
    "Move cursor vertically up ARG candidates.
If the input is empty, select the previous history element instead."
    (interactive "p")
    (if (string= ivy-text "")
        (ivy-previous-history-element 1)
      (ivy-previous-line arg)))
  (use-package iedit
    :init
    (defun pk/swiper-iedit ()
      (interactive)
      (unless (string= ivy-text "")
        (ivy-exit-with-action
         (lambda (_)
           ;; This lambda is basically a copy of `iedit-mode-from-isearch'
           (setq mark-active nil)
           (run-hooks 'deactivate-mark-hook)
           (when iedit-mode
             (iedit-lib-cleanup))
           (let ((result
	              (catch 'not-same-length
	                (iedit-start ivy-text (point-min) (point-max)))))
             (cond ((not iedit-occurrences-overlays)
                    (message "No matches found for %s" ivy-text)
                    (iedit-done))
                   ((equal result 'not-same-length)
                    (message "Matches are not the same length.")
                    (iedit-done)))))))))
  :bind
  (:map global-map
        ("C-s" . #'swiper-isearch)
        ("C-r" . #'swiper-isearch-backward)
   :map swiper-map
        ("C-r" . #'pk/swiper-C-r)
        ("C-c ;" . #'pk/swiper-iedit)))


;; Disable some ido hooks for helm mode
(when exordium-helm-everywhere
  (advice-remove 'call-interactively #'call-interactively@ido-cr+-record-current-command)
  (remove-hook 'minibuffer-setup-hook #'ido-minibuffer-setup))

;; Use python3 for org
(setq org-babel-python-command "python3")
;; Make tabs work nativly in org mode's src blocks
(setq org-src-tab-acts-natively t)

;; (use-package magit-todos
;;   :ensure-system-package (rg . ripgrep)
;;   :config
;;   (magit-todos-mode))

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

(use-package org-tree-slide
  :bind
  (:map org-mode-map
        ("<f8>" . org-tree-slide-mode)
        ("S-<f8>". org-tree-slide-skip-done-toggle)))


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
      "<-" "-->" "->>" ";;" "\\n" "www" ".."
      ;; org-mode ballots -> they are unicode chars, not glyph
      ;; "[ ]" "[X]" "[-]"
      ;; fira sans
      "ffi" "fi" "fl"
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
  :quelpa ((mixed-pitch :fetcher git
                        :url "https://gitlab.com/pkryger/mixed-pitch.git"
                        :branch "different-cursor")
           :upgrade t)
  :when exordium-osx
  :custom
  (mixed-pitch-fixed-pitch-cursor 'box)
  :init
  ;; TODO: move to exordium or to mixed-pitch-mode
  (defcustom pk/mixed-pitch--inhibit-modes
    '(yaml-mode
      nxml-mode)
    "TODO: List of major modes that the `mixed-pitch-mode' should not be activated.
This is useful, when certain modes are derived from modes that `mixed-pitch-mode'
is activated."
    :type 'list)

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
                           '(("+" exordium-org-strike-through (:strike-through t)))))
                        '(org-fontify-whole-heading-line t))

  ;; TODO: move this to exordium
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-date)
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
(use-package company-posframe
  :when exordium-osx
  :diminish
  :bind
  (:map company-posframe-active-map
        ("C-h" . #'company-posframe-quickhelp-toggle)
        ("C-n" . #'company-select-next)
        ("C-p" . #'company-select-previous))
  :config
  (company-posframe-mode 1)
  ;; TODO: move this to desktop configuration
  (add-to-list 'desktop-minor-mode-table '(company-posframe-mode nil)))


(add-hook 'git-commit-mode-hook 'turn-on-auto-fill)
(use-package forge
   :config
   (add-to-list 'forge-owned-accounts '("pkryger" . (remote-name "pkryger")))
   (add-to-list 'forge-owned-accounts '("emacs-exordium" . (remote-name "exordium"))))

;; Configure tabs
(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-separator "⎞⎛")
  (tab-bar-tab-hints t)
  (tab-bar-select-tab-modifiers '(hyper))
  (tab-bar-show t)
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

(use-package so-long
  :config
  (setq-default bidi-paragraph-direction 'left-to-right)
  (when (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))
  (global-so-long-mode))

(quelpa '(basic-stats :fetcher github
                      :repo "pkryger/basic-stats"
                      :branch "main"))
;;
