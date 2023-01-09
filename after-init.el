;; -*- lexical-binding: t; -*-
;;; Code:

(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))

(use-package modus-themes
  :init
  (require 'modus-themes nil t)
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-completions '((matches . (extrabold background intense))
                                   (selection . (semibold accented intense))
                                   (popup . (semibold accented)))
        modus-themes-org-blocks 'gray-background
        modus-themes-headings (let* ((low-level-properties '(variable-pitch rainbow regular))
                                     (high-level-properties `(,@low-level-properties overline)))
                                `((1 . (,@high-level-properties 1.27))
                                  (2 . (,@high-level-properties 1.21))
                                  (3 . (,@high-level-properties 1.15))
                                  (4 . (,@high-level-properties 1.1))
                                  (t . (,@low-level-properties))))
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-common-palette-overrides
        `((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (bg-mode-line-active bg-blue-subtle)
          (fg-mode-line-active fg-main)
          (fg-region unspecified)
          ,@modus-themes-preset-overrides-faint))

  (defun pk/modus-themes--custom-faces ()
    ;; The following is an expanded macro `modus-themes-with-colors',
    ;; but need it so that it will evaluate when function is executed
    ;; -- begin of macro preface --
    (let* ((sym (gensym))
           (colors (mapcar #'car (modus-themes--current-theme-palette))))
      (eval
       `(let* ((c '((class color) (min-colors 256)))
               (,sym (modus-themes--current-theme-palette :overrides))
               ,@(mapcar (lambda (color)
                           (list color
                                 `(let* ((value (car (alist-get ',color ,sym))))
                                    (if (or (stringp value)
                                            (eq value 'unspecified))
                                        value
                                      (car (alist-get value ,sym))))))
                         colors))
          (ignore c ,@colors)
          ;; -- end of macro preface --

          ;; helm-rg uses ansi colours from rg output to highlight matches,
          ;; unfortunateally this doesn't allow for custom overrides so
          ;; use advice to hack around
          ;; It seems that support for defcustom has been added in the past:
          ;; https://github.com/cosmicexplorer/helm-rg/blob/1f01b2f/helm-rg.el#L931-L935
          ;; but then subsequently deleted in
          ;; https://github.com/cosmicexplorer/helm-rg/commit/2221701
          (advice-add 'helm-rg--construct-match-text-properties :override
                      `(lambda (&rest _)
                         (list 'ansi-color-bold (list ':foreground ,red))))

          (custom-theme-set-faces
           'user
           `(fixed-pitch ((t (,@c :family ,(face-attribute 'default :family) :height ,(face-attribute 'default :height)))))
           `(fill-column-indicator ((t (,@c :height 1 :background ,bg-main :foreground ,bg-inactive))))
           `(exordium-org-work ((t (,@c :inherit 'org-todo :foreground ,rust))))
           `(exordium-org-wait ((t (,@c :inherit 'org-todo :foreground ,cyan))))
           `(iedit-occurrence
             ((t (,@c :inherit nil
                      :box (:line-width -2 :color ,(face-attribute 'modus-themes-completion-match-0 :foreground))))))
           `(iedit-read-only-occurrence
             ((t (,@c :inherit nil
                      :box (:line-width -2 :color ,(face-attribute 'modus-themes-completion-match-1 :foreground))))))
           `(aw-leading-char-face ((t (,@c :foreground ,red :bold t :height 1.5))))
           ;; Redoing helm, inspired by last removed version in:
           ;; https://github.com/protesilaos/modus-themes/commit/1efaa7ef79682ec13493351d52ed1b339fb6ace2
           `(helm-selection ((t (,@c :inherit modus-themes-completion-selected))))
           `(helm-match ((t (,@c :inherit modus-themes-completion-match-0))))
           `(helm-match-item ((t (,@c :inherit modus-themes-completion-match-0))))
           `(helm-visible-mark ((t (,@c :inherit modus-themes-subtle-cyan))))
           `(helm-source-header ((t (,@c :inherit bold :foreground ,fg-main))))
           `(helm-header-line-left-margin ((t (,@c :inherit bold :foreground ,yellow-intense))))
           `(helm-candidate-number ((t (,@c :foreground ,cyan))))
           `(helm-candidate-number-suspended ((t (,@c :foreground ,yellow))))
           `(helm-locate-finish ((t (,@c :inherit success))))
           `(helm-swoop-target-word-face ((t (,@c :inherit modus-themes-completion-match-0))))
           `(helm-swoop-target-line-face ((t (,@c :inherit highlight :extend t))))
           `(helm-swoop-target-line-block-face ((t (,@c :inherit highlight :extend t))))
           `(helm-moccur-buffer ((t (,@c :inherit bold :foreground ,name))))
           `(helm-resume-need-update ((t (,@c :inherit pulse-highlight-start-face))))
           `(helm-grep-command ((t (,@c :inherit helm-source-header))))
           `(helm-grep-match ((t (,@c :inherit modus-themes-completion-match-0))))
           `(helm-grep-lineno ((t (,@c :inherit shadow))))
           `(helm-grep-finish ((t (,@c :inherit bold))))
           `(helm-buffer-archive ((t (,@c :inherit bold :foreground ,cyan))))
           `(helm-buffer-directory ((t (,@c :inherit bold :foreground ,blue))))
           `(helm-buffer-file ((t (,@c :foreground ,fg-main))))
           `(helm-buffer-modified ((t (,@c :foreground ,yellow-warmer))))
           `(helm-buffer-not-saved ((t (,@c :foreground ,red-warmer))))
           `(helm-buffer-process ((t (,@c :foreground ,magenta))))
           `(helm-buffer-saved-out ((t (,@c :inherit bold :background ,bg-cyan-nuanced :foreground ,red))))
           `(helm-buffer-size ((t (,@c :inherit shadow))))
           `(helm-ff-backup-file ((t (,@c :inherit shadow))))
           `(helm-ff-denied ((t (,@c :inherit modus-themes-intense-red))))
           `(helm-ff-directory ((t (,@c :inherit helm-buffer-directory))))
           `(helm-ff-dirs ((t (,@c :inherit bold :foreground ,blue-cooler))))
           `(helm-ff-dotted-directory ((t (,@c :inherit bold :background ,bg-cyan-nuanced :foreground ,fg-alt))))
           `(helm-ff-dotted-symlink-directory ((t (,@c :inherit (button helm-ff-dotted-directory)))))
           `(helm-ff-executable ((t (,@c :foreground ,magenta-warmer))))
           `(helm-ff-file ((t (,@c :foreground ,fg-main))))
           `(helm-ff-file-extension ((t (,@c :foreground ,yellow-warmer))))
           `(helm-ff-invalid-symlink ((t (,@c :foregrond ,red :underline ,red-faint))))
           `(helm-ff-pipe ((t (,@c :foreground ,red-warmer))))
           `(helm-ff-prefix ((t (,@c :background ,bg-ochre :foreground ,magenta))))
           `(helm-ff-socket ((t (,@c :foreground ,red-cooler))))
           `(helm-ff-suid ((t (,@c :foreground ,red-intense))))
           `(helm-ff-symlink ((t (,@c :foreground ,cyan :underline ,cyan-faint))))
           `(helm-ff-truename ((t (,@c :foreground ,blue-cooler))))
           `(helm-history-deleted ((t (,@c :inherit helm-ff-invalid-symlink))))
           `(helm-history-remote ((t (,@c :foreground ,red-cooler))))
           `(helm-delete-async-message ((t (,@c :inherit bold :foreground ,magenta))))
           `(helm-ff-rsync-progress ((t (,@c :inherit bold :foreground ,red-warmer))))
           `(helm-rg-match-text-face ((t (,@c :inherit modus-themes-completion-match-0))))
           `(helm-rg-line-number-match-face ((t (,@c :inherit helm-grep-lineno))))
           `(helm-rg-file-match-face ((t (,@c :inherit helm-moccur-buffer))))
           `(helm-rg-preview-line-highlight ((t (,@c :inherit highlight :extend t))))
           `(helm-rg-title-face ((t (,@c :inherit helm-source-header))))
           `(helm-rg-base-rg-cmd-face ((t (,@c :inherit helm-source-header))))
           `(helm-rg-active-arg-face ((t (,@c :foreground ,yellow-warmer))))
           `(helm-rg-inactive-arg-face ((t (,@c :foreground ,fg-dim))))
           `(helm-rg-extra-arg-face ((t (,@c :foreground ,yellow-cooler))))
           `(helm-rg-directory-cmd-face ((t (,@c :inherit helm-ff-directory))))
           `(helm-rg-directory-header-face ((t (,@c :inherit helm-ff-directory))))
           `(helm-M-x-key ((t (,@c :inherit modus-themes-key-binding))))
           `(helm-M-x-short-doc ((t (,@c :inherit completions-annotations)))))))))

  :hook
  (modus-themes-after-load-theme . pk/modus-themes--custom-faces)

  :config
  (load-theme 'modus-operandi :no-confirm)
  (pk/modus-themes--custom-faces)

  :bind
  ("<f5>" . modus-themes-toggle))


(unless (getenv "ci_tests")
(use-package tree-sitter-langs)
(use-package tree-sitter
  :diminish
  :after (tree-sitter-langs)
  ;; TODO: check out themes, likely in modus-themes
  ;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/blob/master/doc/emacs-tree-sitter.org#theming
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode)
  :custom
  (font-lock-maximum-decoration t)
  :config
  (when-let ((language-name (alist-get 'ruby-mode
                                       tree-sitter-major-mode-language-alist)))
    (add-to-list 'tree-sitter-major-mode-language-alist
                 (cons 'enh-ruby-mode language-name)))
  (global-tree-sitter-mode))
)


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

  (setq mac-emulate-three-button-mouse t)
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
  ((after-init . mac-pseudo-daemon-mode)
   (after-init . server-start)))

(setq disabled-command-function nil)
(setq confirm-kill-emacs 'y-or-n-p)
(setq-default display-line-numbers-widen t)

;; See `split-window-sensibly' doc...
;; this setup basically forces a vertical split, but only up to 2 windows in a frame
(setq split-height-threshold nil)
(setq split-width-threshold 160)

(use-package use-package-ensure-system-package)
(use-package quelpa-use-package)

;; ITERM2 MOUSE SUPPORT from https://www.emacswiki.org/emacs/iTerm2
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (_)))

(defconst pk/shrug-string "¯\\_(ツ)_/¯")

(defun pk/shrug ()
  "Insert ¯\\_(ツ)_/¯ at point."
  (interactive)
  (insert pk/shrug-string))

(defun pk/shrug-as-kill ()
  "Add ¯\\_(ツ)_/¯ to kill buffer."
  (interactive)
  (kill-new pk/shrug-string))

(use-package helm
  :diminish
  :custom
  (helm-split-window-other-side-when-one-window 'right)
  (helm-M-x-show-short-doc t)
  :bind
  (("C-x b" . #'helm-buffers-list)
   :map ctl-x-map
        ("b" . #'helm-buffers-list)
   :map helm-command-map
        ("g" . #'helm-google-suggest)))


(use-package elisp-mode
  :ensure nil
  :bind
  (:map emacs-lisp-mode-map
        ;; Follow Anaconda default mapping
        ("M-." . #'xref-find-definitions)
        ("M-," . #'xref-pop-marker-stack)
        ("M-r" . #'xref-find-references)
        ("M-?" . #'helpful-at-point)))

(use-package eglot)
(use-package eldoc
  :ensure nil
  :custom
  (eldoc-idle-delay 0.25)
  (eldoc-echo-area-prefer-doc-buffer t))

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :bind
  (:map c-mode-base-map
        ;; Override exordium's greedy rtags
        ("M-." . #'xref-find-definitions)
        ("M-," . #'xref-pop-marker-stack)))


(use-package orderless
  :custom
  (completion-styles '(orderless)))

(use-package ispell
;;   :ensure-system-package aspell
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

  :config
  (setq flyspell-issue-message-flag nil)

  :hook
  ((git-commit-mode . flyspell-mode)
   (org-mode        . flyspell-mode)
   (text-mode       . flyspell-mode))

  :bind (:map flyspell-mode-map
         ("C-;" . flyspell-correct-wrapper)
         ([(control ?\,)] . nil)
         ([(control ?\.)] . nil)))

(use-package flyspell-correct-helm
  :after (flyspell)
  :custom
  (flyspell-correct-interface #'flyspell-correct-helm))


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


(use-package jenkinsfile-mode
  :after (flycheck)
  :config
  (flycheck-add-mode 'groovy 'jenkinsfile-mode))

(use-package groovy-mode
  :after (yasnippet projectile)
  :init
  (defun pk/groovy-mode--create-test-files ()
    (setq-local projectile-create-missing-test-files t))
  :hook
  ((groovy-mode . yas-minor-mode)
   (groovy-mode . pk/groovy-mode--create-test-files))
  :config
  (projectile-register-project-type 'pbnj '("pom.xml" "pbnj_lib_config.yaml")
                                    :project-file "pom.xml"
                                    :compile "pbnj_release_brancher --branch=release-dev --remote=pkryger"
                                    :test "mvn --batch-mode verify"
                                    :test-suffix "Test"
                                    :src-dir "src/"
                                    :test-dir "test/")

  (add-to-list 'yas-snippet-dirs
               (concat (file-name-directory (or load-file-name
                                                (buffer-file-name))) "snippets")))


(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yaml\\.template\\'" . yaml-mode)))

(use-package flycheck
  :custom
  (flycheck-global-modes '(not c++-mode c-mode org-mode))
  :hook
  (after-init . global-flycheck-mode))


(use-package compile
  :custom
  (compilation-scroll-output 'first-error))
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


(use-package posframe)
(use-package ace-window
  :after (posframe)
  :diminish "AW"
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-translate-char-function #'(lambda (c)
                                  (if (eq ?\M-o c) ?n c)))
  :config
  (global-set-key (kbd "M-o") #'ace-window)
  (when (and (require 'posframe nil t) (posframe-workable-p))
    (ace-window-posframe-mode)))


(defconst pk/desktop-files-not-to-save
  (rx-let ((path (+ (or alnum digit "." "/" "-" "_" "~"))))
    (rx (or (seq string-start "/" (zero-or-more (not (any "/" ":"))) ":")
            (seq "(ftp)" string-end)
            (seq string-start path "/emacs/" path "/lisp/" path
                 ".el.gz" string-end)
            (seq string-start path "/.emacs.d/elpa/" path
                 ".el" string-end)))))

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
  (add-to-list 'desktop-modes-not-to-save 'magit-mode)
  (add-to-list 'desktop-modes-not-to-save 'magit-log-mode)
  :custom
  (desktop-files-not-to-save pk/desktop-files-not-to-save)
  (desktop-restore-eager 8))


(defun pk/project-enable-flycheck-python-mypy ()
  "Enable `python-mypy' checker in current project."
  (interactive)
  (mapc (lambda (buf)
          (with-current-buffer buf
            (when (and (eq major-mode 'python-mode)
                       flycheck-mode)
              (flycheck--toggle-checker 'python-mypy t)
              (flycheck-buffer))))
        (projectile-project-buffers)))

(defun pk/python-site-packages-wip ()
  ;; TODO: doesn't work in venv with pythonpath
  (s-split
   ", "
   (s-replace-all
    '(("'" . ""))
    (substring
     (s-trim (shell-command-to-string
              (concat python-shell-interpreter " -c 'import sys; print(sys.path)'")))
     1 -1))))

(defcustom pk/python-bootstrap-packages
  '("pytest" "mypy")
  "List of packages to install as a part of `python-bootstrap'.
N.B. The `mypy' is optional but necessary for `flycheck'.
     The `pytest' is optional but necessary for `python-pytest'."
  :type 'list)

(defcustom pk/python-bootstrap-requirements
  '("requirements-dev.txt" "requirements.txt")
  "List of requirements files to use as a part of `python-bootstrap'.
The first file found in a project will be used."
  :type 'list)

(use-package python
  :ensure nil
  :custom
  (python-shell-interpreter  "python3")
  ;; :hook
  ;; (python-mode . (lambda ()
  ;;                  (setq fill-column 100)))
  )

(use-package toml
  :quelpa ((toml :fetcher git
                 :url "https://github.com/pkryger/emacs-toml.git"
                 :branch "fixes")
           :upgrade t))

;; TODO: use projectile to jump between tests and implementation as well as run tests
(use-package python-pytest
  :bind (:map python-mode-map
              ("C-c t" . python-pytest-dispatch))
  :init
  (setq python-pytest-executable
        (concat python-shell-interpreter " -m pytest")))

(use-package anaconda-mode
  :custom
  (anaconda-mode-use-posframe-show-doc t)
  ;; :hook
  ;; to let to choose eglot
  ;; (python-mode . anaconda-mode)
  ;; (python-mode . anaconda-eldoc-mode)
  )

(use-package company-anaconda
  :after (company)
  :config
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

;; See https://blog.adam-uhlir.me/python-virtual-environments-made-super-easy-with-direnv-307611c3a49a
;; for layout thing
(use-package direnv
;;   :ensure-system-package direnv
  :config
  (direnv-mode))

;; TODO: investigate using of `.dir-locals.el', i.e., sth like
;; ((python-mode . ((fill-column . 88)
;;                  (eval . (progn
;;                            (when-let ((venv (getenv "VIRTUAL_ENV")))
;;                              (setq-local python-shell-virtualenv-root
;;                                          (pythonic-python-readable-file-name venv))
;;                              (when (string-prefix-p venv (buffer-file-name))
;;                                (read-only-mode)
;;                                (flycheck-mode -1)))
;;                            (setq-local python-shell-extra-pythonpaths
;;                                        (list
;;                                         (concat (projectile-project-root) "src")
;;                                         (concat (projectile-project-root) "tests")))
;;                            (python-black-on-save-mode)
;;                            (python-isort-on-save-mode))))))
;; TODO: investigate installing when only `setup.cfg' is available, with something like:
;; $ pip install -e '.[testing,docs,format]'

(defcustom pk/python-inhibit-install-packages '("libnlp" "scipy" "scikit-learn")
  "List of packages that should not be installed while bootstrapping.")

(defun pk/python--packages-from-in-files (project-root)
  "Return list of constrained packages form *.in files in PROJECT-ROOT."
  (let ((requirements (f-join project-root "requirements.in")))
    (mapcar
     (lambda (package-spec)
       (format "'%s'" (s-replace-regexp "\\W*;.*$" "" package-spec)))
     (seq-filter
      (lambda (line)
        (not
         (or (s-starts-with-p "#" line)
             (s-matches-p "^\\W*$" line)
             (cl-find-if (lambda (package-spec)
                           (s-matches-p (format
                                         "^%s\\(?:\\W*[!<>=]=?\\W*[0-9\.]+\\)?\\W*\\(?:;.*\\)?$"
                                         package-spec)
                                        line)) ; see if the package is not inhibited
                         pk/python-inhibit-install-packages))))
      (cl-mapcan
       (lambda (path)
         (s-split "\n" (f-read path)))
       (nconc (when (f-exists-p requirements)
                (list requirements))
              (f-glob "*.in" (f-join project-root "requirements"))))))))

(defun pk/python--packages-from-file (file)
  "Return list of constrained packages form FILE."
  ;; TODO: need to cut out not matching python_version
  (mapcar
   (lambda (package-spec)
     (format "'%s'" (s-replace-regexp "\\W*;.*$" "" package-spec)))
   (seq-filter
    (lambda (line)
      (not
       (or (s-starts-with-p "#" line)
           (s-matches-p "^\\W*$" line)
           (cl-find-if (lambda (package-spec)
                         (s-matches-p (format
                                       "^%s\\(?:\\W*[!<>=]=?\\W*[0-9\.]+\\)?\\W*\\(?:;.*\\)?$"
                                       package-spec)
                                      line))
                       pk/python-inhibit-install-packages))))
    (s-split "\n" (f-read file)))))

;; (let ((project-root "/Users/pkryger/ainews/paws-inference"))
;;   (s-join " "
;;           (pk/python--packages-from-file (f-join project-root "requirements.in")))
;;   (s-join " "
;;           (cl-mapcan (lambda (path)
;;                        (pk/python--packages-from-file path))
;;                      (f-glob "*.in" (f-join project-root "requirements"))))
;;   (s-join " "
;;           (cl-mapcan (lambda (path)
;;                        (pk/python--packages-from-file path))
;;                      (f-glob "*.in" (f-join project-root "requirements-dev")))))

(defun pk/python-bootstrap (dir)
  "In a given `DIR' bootstrap python environment.
Such a bootstrap will provide a dedicated virtual environment via `direnv'
python layout with:
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
            (insert (concat "layout " python "\n"))))
        (progress-reporter-update
         reporter (cl-incf progress) "[allowing direnv...]")
        (direnv-allow)
        (when-let ((buf (get-buffer "*Shell Command Output*")))
          (with-current-buffer buf
            (erase-buffer)))
        (dolist (package pk/python-bootstrap-packages)
          (progress-reporter-update
           reporter (cl-incf progress) (format "[installing %s...]" package))
          (let ((shell-command-dont-erase-buffer 'end-last-out))
            (shell-command (concat (s-join " " pip-command) " " package))))
        (when requirements
          (progress-reporter-update
           reporter (cl-incf progress) (format "[installing from %s...]"
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
(diminish 'git-gutter-mode)
(diminish 'company-mode)


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
  (("C-s" . swiper-isearch)
   ("C-r" . swiper-isearch-backward)
   :map swiper-map
        ("C-r" . pk/swiper-C-r)
        ("C-c ;" . pk/swiper-iedit)))


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
  :custom
  (ess-ask-for-ess-directory nil) ; workaround for helm not being able to start
  :config
  (require 'ess-r-mode)
  (remove-hook 'ess-r-mode-hook 'ess-r-setup-flymake)
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
        ;; TODO: probably use Basic for everything, like in pk/jaas-auth-header
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
      "<<<" "<=" "<=>" "=" "==" ">" ">=" ">>" ">>>" "?:" "\\\\"
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


;; TODO: move to exordium and likely hide behind the
;; `exordium-use-variable-pitch' and `exordium-complete-mode' set to `:company'
(use-package company-posframe
  :after (posframe)
  :when exordium-osx
  :diminish
  :bind
  (:map company-posframe-active-map
        ("C-h" . #'company-posframe-quickhelp-toggle)
        ("C-n" . #'company-select-next)
        ("C-p" . #'company-select-previous))
  :custom
  (company-posframe-quickhelp-delay 0.2)
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
  :init
  (defun pk/tab-bar--separator (type current)
    "Return a `tab-bar' separator in a form of a propertized  \" ¦ \".

When TYPE is 'first and not in `tab-bar-history-mode' or TYPE is
'first-history the leading space will be omitted.  When type is LAST the
trailing space will be omitted.

When CURRENT is 'this then trailing space and the ?¦ will have
'tab-bar-tab face property.  When CURRENT is 'previous then
leading space and the ?¦ will have 'tab-bar-tab face property.
All the reminder parts of the separator will have
'tab-bar-tab-inactive face property."
    (concat
     (unless (or (eq type 'first-history)
                 (and (not tab-bar-history-mode)
                      (eq type 'first)))
       (propertize " " 'face (if (eq current 'previous)
                                 'tab-bar-tab
                               'tab-bar-tab-inactive)))
     (propertize "⦙" 'face (if current ; alternative: "¦"
                               'tab-bar-tab
                             'tab-bar-tab-inactive))
     (unless (eq type 'last)
       (propertize " " 'face (if (eq current 'this)
                                 'tab-bar-tab
                               'tab-bar-tab-inactive)))))

  :custom
  (tab-bar-separator #'pk/tab-bar--separator)
  (tab-bar-tab-hints t)
  (tab-bar-select-tab-modifiers '(hyper))
  (tab-bar-show t)

  :config
  (define-advice tab-bar-make-keymap-1 (:override (&rest _args) pk/tab-bar-make-keymap-1)
    (let* ((separator (or tab-bar-separator (if window-system " " "|")))
           (i 0)
           (tabs (funcall tab-bar-tabs-function))
           previous-current)
      (append
       '(keymap (mouse-1 . tab-bar-mouse-down-1))
       (when tab-bar-history-mode
         `((sep-history-back menu-item ,(if (functionp separator)
                                            (funcall separator 'first-history nil)
                                          separator)
                             ignore)
           (history-back
            menu-item ,tab-bar-back-button tab-bar-history-back
            :help "Click to go back in tab history")
           (sep-history-forward menu-item ,(if (functionp separator)
                                               (funcall separator 'mid-history nil)
                                             separator)
                                ignore)
           (history-forward
            menu-item ,tab-bar-forward-button tab-bar-history-forward
            :help "Click to go forward in tab history")))
       (mapcan
        (lambda (tab)
          (setq i (1+ i))
          (append
           `((,(intern (format "sep-%i" i)) menu-item
              ,(if (functionp separator)
                   (funcall separator
                            (when (eq i 1) 'first)
                            (cond
                             ((eq (car tab) 'current-tab) 'this)
                             (previous-current 'previous)))
                 separator)
              ignore))
           (cond
            ((eq (car tab) 'current-tab)
             (setq previous-current t)
             `((current-tab
                menu-item
                ,(propertize (concat (if tab-bar-tab-hints (format "%d " i) "")
                                     (alist-get 'name tab)
                                     (or (and tab-bar-close-button-show
                                              (not (eq tab-bar-close-button-show
                                                       'non-selected))
                                              tab-bar-close-button) ""))
                             'face 'tab-bar-tab)
                ignore
                :help "Current tab")))
            (t
             (setq previous-current nil)
             `((,(intern (format "tab-%i" i))
                menu-item
                ,(propertize (concat (if tab-bar-tab-hints (format "%d " i) "")
                                     (alist-get 'name tab)
                                     (or (and tab-bar-close-button-show
                                              (not (eq tab-bar-close-button-show
                                                       'selected))
                                              tab-bar-close-button) ""))
                             'face 'tab-bar-tab-inactive)
                ,(or
                  (alist-get 'binding tab)
                  `(lambda ()
                     (interactive)
                     (tab-bar-select-tab ,i)))
                :help "Click to visit tab"))))
           `((,(if (eq (car tab) 'current-tab) 'C-current-tab (intern (format "C-tab-%i" i)))
              menu-item ""
              ,(or
                (alist-get 'close-binding tab)
                `(lambda ()
                   (interactive)
                   (tab-bar-close-tab ,i)))))))
        tabs)
       `((sep-add-tab menu-item
                      ,(if (functionp separator)
                           (funcall separator 'last (if previous-current 'previous))
                         separator)
                      ignore))
       (when (and tab-bar-new-button-show tab-bar-new-button)
         `((add-tab menu-item ,tab-bar-new-button tab-bar-new-tab
                    :help "New tab"))))))

  :bind
  (("M-<tab>" . tab-next)
   ("M-S-<tab>" . tab-previous)
   ("H-t" . tab-new)))



;; evil likes to turn itself on, let's disable it so no surprises
(use-package evil
  :init
  (defun pk/disable-evil-mode ()
    (unless exordium-enable-evil-mode
      (when (or evil-mode evil-local-mode)
        (turn-off-evil-mode))))
  :hook
  ((evil-mode . pk/disable-evil-mode)
   (evil-local-mode . pk/disable-evil-mode))
  :config
  (advice-remove #'select-window #'ad-Advice-select-window))


(use-package so-long
  :config
  (setq-default bidi-paragraph-direction 'left-to-right)
  (when (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))
  (global-so-long-mode))


(use-package git-link
  :after (magit transient)
  :custom
  (git-link-use-commit t)
  :config
  (transient-append-suffix 'magit-file-dispatch "g" '(1 "f" "copy link" git-link)))


(defun pk/insert-link-dwim (make-link-string insert-link &optional arg)
  "Insert a link with a personal dwim preferences.

Use MAKE-LINK-STRING to create a link string to be inserted.
Use INSERT-LINK with ARG to prompt user for a link to be inserted.

Based on https://xenodium.com/emacs-dwim-do-what-i-mean/"
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (funcall make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-in-link))
           (insert (funcall
                    make-link-string
                    clipboard-url
                    (read-string "title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (let ((current-prefix-arg arg))
             (call-interactively insert-link))))))

(defun pk/markdown--make-link-string (link &optional description)
  "Make a markdown link consisting of LINK and DESCRIPTION.

When LINK is a [REF] return a reference link in a form [DESCRIPTION][REF].
When no DESCRIPTION return a link in a form <LINK>.
Otherwise return a link in a form [DESCRIPTION](LINK)."
  (if (not description)
      (concat "<" link ">")
    (concat "[" description "]"
            (if (string-match "\\`\\[\\(.*\\)\\]\\'" link)
                link
              (concat "(" link ")")))))

(defun pk/org-insert-link-dwim (&optional complete-file)
  "Like `org-insert-link' but with dwim features.

See `org-insert-link' for COMPLETE-FILE description.
Based on https://xenodium.com/emacs-dwim-do-what-i-mean/"
  (interactive "P")
  (pk/insert-link-dwim 'org-link-make-string 'org-insert-link complete-file))

(defun pk/markdown-insert-link-dwim ()
  "Like `markdown-insert-link' but with dwim features.

Based on https://xenodium.com/emacs-dwim-do-what-i-mean/"
  (interactive)
  (pk/insert-link-dwim 'pk/markdown--make-link-string 'markdown-insert-link))

(use-package org
  :bind
  (:map org-mode-map
        ([remap org-insert-link] . #'pk/org-insert-link-dwim)))

(use-package markdown-mode
  :bind
  (:map markdown-mode-map
        ([remap markdown-insert-link] . #'pk/markdown-insert-link-dwim)))


(use-package mode-line-bell
  :config
  (mode-line-bell-mode))

(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t))

(use-package diredfl
  :config
  (diredfl-global-mode))

(use-package dired-du
  :custom
  (dired-du-used-space-program `(,(if exordium-osx "gdu" "du") "-sb")))

(use-package ob-async)

(use-package transpose-frame)

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package protobuf-mode)

(use-package apheleia
  :config
  (setf (alist-get 'black apheleia-formatters)
        '("black" "--line-length" (number-to-string fill-column) "-"))
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(isort black)))

;; Cassandra - CQL support with sql-mode and ob-sql
(require 'sql)
(require 'org)

(defun pk/sql--custom-set-product-feature-factory (feature)
  "Create a custom set function that update the FEATURE for Cassandra in `sql'."
  #'(lambda (sym value)
      (sql-set-product-feature 'cql
                               feature
                               (if (member feature sql-indirect-features)
                                   sym
                                 value))
      (set-default sym value)))
:config
(setf sql-product-alist (assoc-delete-all 'cql sql-product-alist))
(sql-add-product 'cql "Cassandra"
                 :free-software t)

;; CQL keywords from (without ANSI keywords):
;; https://cassandra.apache.org/doc/latest/cassandra/cql/appendices.html#appendix-A
(defvar pk/sql-cql-font-lock-keywords
  `(,(sql-font-lock-keywords-builder 'font-lock-type-face nil
                                     "ascii" "bigint" "counter" "inet" "text" "timeuuid" "tinyint" "uuid" "varint"
                                     "bitstring" "byte" "complex" "enum" "macaddr")
    ,(sql-font-lock-keywords-builder 'font-lock-keyword-face nil
                                     "allow" "apply" "authorize" "batch" "clustering" "columnfamily" "compact"
                                     "counter" "custom" "entries" "filtering" "finalfunc" "frozen" "functions"
                                     "if" "index" "infinity" "initcond" "json" "keys" "keyspace" "keyspaces" "list"
                                     "login" "nan" "nologin" "norecursive" "nosuperuser" "password" "permission"
                                     "permissions" "rename" "replace" "roles" "sfunc" "storage" "stype" "superuser"
                                     "timeuuid" "token" "truncate" "ttl" "tuple" "unlogged" "use" "users" "writetime"
                                     "maxwritetime"))
  "Cassandra CQL keywords used by font-lock.")
(sql-set-product-feature 'cql
                         :font-lock 'pk/sql-cql-font-lock-keywords)

;; C-style comments // and /**/ (the latter is supported by `sql-mode')
(sql-set-product-feature 'cql
                         :syntax-alist '((?/ . ". 124")))

(defcustom pk/sql-cql-program "cqlsh"
  "Command to start cqlsh."
  :type 'file
  :group 'SQL
  :set (pk/sql--custom-set-product-feature-factory :sqli-program))
(defcustom pk/sql-cql-login-params '(user password host port)
  "List of login parameters needed to connect to Cassandra."
  :type 'sql-login-params
  :group 'SQL
  :set (pk/sql--custom-set-product-feature-factory :login-params))
(defcustom pk/sql-cql-options nil
  "List of additional options for `pk/sql-cql-program'."
  :type '(repeat string)
  :group 'SQL
  :set (pk/sql--custom-set-product-feature-factory :sqli-options))

(sql-set-product-feature 'cql
                         :prompt-regexp
                         (rx bol
                             (zero-or-one (any "a-z0-9_") "@")
                             "cqlsh"
                             (zero-or-one ":" (any "a-z") (any "a-z0-9"))
                             ">"))
(sql-set-product-feature 'cql
                         :prompt-length 7)
(sql-set-product-feature 'cql
                         :prompt-cont-regexp "^   \\.\\.\\.")
(sql-set-product-feature 'cql
                         :list-all "DESCRIBE TABLES;")
(sql-set-product-feature 'cql
                         :list-table "DESCRIBE %s;")

(defun pk/sql-comint-cql (product params &optional buf-name)
  "Create comint buffer and connect to Cassandra.

PRODUCT is the SQL product.  PARAMS is a list of strings which are
passed as command line arguments.  BUF-NAME is the name of the new
buffer.  If nil, a name is chosen for it."
  (let ((params
         (append
          (if (not (string= "" sql-user))
              (list "-u" sql-user))
          (if (not (string= "" sql-password))
              (list "-p" sql-password))
          params
          (when (not (string= "" sql-server))
            (list sql-server)
            (if (not (= 0 sql-port))
                (list (number-to-string sql-port)))))))
    (sql-comint product params buf-name)))
(sql-set-product-feature 'cql
                         :sqli-comint-func 'pk/sql-comint-cql)

(defun pk/sql-cql (&optional buffer)
  "Run cqlsh by Cassandra as an inferior process in the BUFFER."
  (interactive "P")
  (sql-product-interactive 'cql buffer))

(defun pk/sql-mode-with-cql-product ()
  "Enter `sql-mode' with the `cql' product set."
  (setq-local sql-product 'cql)
  (sql-mode))

(add-to-list 'auto-mode-alist '("\\.cql\\'" . pk/sql-mode-with-cql-product))


;; BEGIN: a hacky way to get font-lock per engine in org-mode
(defvar pk/sql--ob-fontify-engine nil)
(defvar pk/sql--ob-fontify-product-orig nil)

(defun pk/sql--ob-fontify-init-engine (limit)
  "Extract `:engine' from an `org-mode' block SRC block.

This is intended to be used as an advise for
`org-fontify-meta-lines-and-blocks'.  LIMIT has the same meaning
as the advised function."
  (when org-src-fontify-natively
    (unless pk/sql--ob-fontify-product-orig
      (setq pk/sql--ob-fontify-product-orig sql-product))
    (save-excursion
      (setq pk/sql--ob-fontify-engine
            ;; TODO: use (org-babel-get-src-block-info t), see
            ;; https://stackoverflow.com/a/66911315/519827
            (let ((case-fold-search t))
              (when (re-search-forward
                     (rx bol
                         (zero-or-more (any " \t")) "#+begin_src"
                         (one-or-more (any " \t")) "sql"
                         (zero-or-more (one-or-more (any " \t")) (zero-or-more any))
                         (one-or-more (any " \t")) ":engine"
                         (one-or-more (any " \t")) (group (one-or-more (any "a-zA-Z")))
                         (zero-or-more (one-or-more (any " \t")) (zero-or-more any))
                         eol)
                     limit t)
                (match-string-no-properties 1)))))))

(defun pk/sql--ob-fontify-set-product (&rest _)
  "Set `sql-product' according to `:engine' extracted by `pk/sql--ob-fontify-init-engine'.

This is intended to be used as an advise for `org-font-lock-ensure'."
  (when (and org-src-fontify-natively
             (eq major-mode 'sql-mode))
    (sql-set-product (if pk/sql--ob-fontify-engine
                         (if-let ((product (intern pk/sql--ob-fontify-engine))
                                  ((assoc product sql-product-alist)))
                             product
                           'ansi)
                       'ansi)))
  (setq pk/sql--ob-fontify-engine nil))

(defun pk/sql--ob-fontify-reset-product (&rest _)
  "Reset `sql-product' back to the original value.

This is intended to be used as an advise for
`org-fontify-meta-lines-and-blocks'."
  (when pk/sql--ob-fontify-product-orig
    (sql-set-product pk/sql--ob-fontify-product-orig)
    (setq pk/sql--ob-fontify-product-orig nil)))

(advice-add 'org-fontify-meta-lines-and-blocks :before #'pk/sql--ob-fontify-init-engine)
;;(advice-add 'org-html-fontify-code :before #'pk/sql--ob-fontify-init-engine)
(advice-add 'org-font-lock-ensure :before #'pk/sql--ob-fontify-set-product)
(advice-add 'org-fontify-meta-lines-and-blocks :after #'pk/sql--ob-fontify-reset-product)
;;(advice-add 'org-html-fontify-code :after #'pk/sql--ob-fontify-reset-product)
;; END: a hacky way to get font-lock per engine in org-mode


(use-package json-mode
  :config
  (add-hook 'json-mode-hook
            #'(lambda ()
                ;; Disable js2-mode that exordium turns on for `javascrip-mode'
                ;; to disable js-lint
                (js2-minor-mode -1))))

;; From Steve Yegge's emacs: https://sites.google.com/site/steveyegge2/my-dot-emacs-file

(defun pk/rename-file-and-buffer (new-name)
 "Rename both current buffer and file it's visiting to NEW-NAME."
 (interactive "sNew name: ")
 (let ((name (buffer-name))
       (filename (buffer-file-name)))
   (if (not filename)
       (message "Buffer '%s' is not visiting a file!" name)
     (if (get-buffer new-name)
         (message "A buffer named '%s' already exists!" new-name)
       (rename-file filename new-name 1)
       (rename-buffer new-name)
       (set-visited-file-name new-name)
       (set-buffer-modified-p nil)))))

(defun pk/move-buffer-file (dir)
 "Move both current buffer and file it's visiting to DIR."
 (interactive "DNew directory: ")
 (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (dir (if (string-match dir "\\(?:/\\|\\\\)$")
                 (substring dir 0 -1)
               dir))
        (new-name (concat dir "/" name)))
   (if (not filename)
       (message "Buffer '%s' is not visiting a file!" name)
     (copy-file filename new-name 1)
     (delete-file filename)
     (set-visited-file-name new-name)
     (set-buffer-modified-p nil))))


(use-package graphviz-dot-mode)

;; what variables should be updated has been copied from exec-path-from-shell-setenv
(defun pk/fixup-path-setenv ()
  "Fixup PATH variable: my bin, homebrew bin, reminder of the elements."
  (interactive)
  (let* ((home (getenv "HOME"))
         (brew-prefix (when-let ((brew (executable-find "brew")))
                        (string-trim (shell-command-to-string (concat brew " --prefix")))))
         (path (mapconcat
                #'identity
                (let* ((dirs (split-string (getenv "PATH") path-separator))
                       (homies (seq-filter (lambda (elt)
                                             (string-prefix-p home elt))
                                           dirs))
                       (brewies (and brew-prefix
                                     (seq-filter (lambda (elt)
                                                   (string-prefix-p brew-prefix elt))
                                                 dirs))))
                  (seq-uniq
                   (append homies
                           brewies
                           (seq-remove (lambda (elt)
                                         (or (member elt homies)
                                             (member elt brewies)))
                                       dirs))
                   #'string=))
                ":")))
    (setenv "PATH" path)
    (setq exec-path (append (parse-colon-path path) (list exec-directory)))
    ;; `eshell-path-env' is a buffer local variable, so change its default
    ;; value.
    (setq-default eshell-path-env path)))

(use-package persistent-scratch
  :init
  (defun pk/persistent-scratch--scratch-buffer-p ()
    "Return non nil when the buffer is a scratch like buffer.
I.e., created with `scratch' or named scratch-"
    (let ((buffer-name (buffer-name)))
      (or (string= "*scratch*" buffer-name)
          (string-prefix-p "scratch-" buffer-name))))
  :custom
  (persistent-scratch-scratch-buffer-p-function #'pk/persistent-scratch--scratch-buffer-p)
  :config
  (persistent-scratch-setup-default))

(quelpa '(basic-stats :fetcher github
                      :repo "pkryger/basic-stats"
                      :branch "main"))

;;
