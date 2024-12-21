;;; after-init.el --- commontap   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-lib)
(exordium-require 'init-prefs)
(exordium-require 'init-environment)


(defgroup pk/exordium nil
  "PKs Exordium extras."
  :group 'local)

(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))

(use-package modus-themes
  :unless (bound-and-true-p exordium-theme)
  :functions (pk/modus-themes--on-appearance-change)
  :autoload (modus-themes-load-theme
             modus-themes--current-theme-palette)
  :init
  (require 'modus-themes nil t)
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-completions '((matches . (extrabold background intense))
                                   (selection . (semibold accented intense))
                                   (popup . (semibold accented)))
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

          (setq org-src-block-faces
                `(("clojure" modus-themes-nuanced-magenta)
                  ("clojurescript" modus-themes-nuanced-magenta)
                  ("elisp" modus-themes-nuanced-magenta)
                  ("emacs-lisp" modus-themes-nuanced-magenta)
                  ("lisp" modus-themes-nuanced-magenta)
                  ("scheme" modus-themes-nuanced-magenta)

                  ("c" modus-themes-nuanced-blue)
                  ("c++" modus-themes-nuanced-blue)
                  ("fortran" modus-themes-nuanced-blue)
                  ("java" modus-themes-nuanced-blue)

                  ("awk" modus-themes-nuanced-yellow)
                  ("ipython" modus-themes-nuanced-yellow)
                  ("js" modus-themes-nuanced-yellow)
                  ("perl" modus-themes-nuanced-yellow)
                  ("python" modus-themes-nuanced-yellow)
                  ("r" modus-themes-nuanced-yellow)
                  ("ruby" modus-themes-nuanced-yellow)
                  ("sed" modus-themes-nuanced-yellow)
                  ("sh" modus-themes-nuanced-yellow)
                  ("shell" modus-themes-nuanced-yellow)

                  ("dot" modus-themes-nuanced-green)
                  ("html" modus-themes-nuanced-green)
                  ("latex" modus-themes-nuanced-green)
                  ("org" modus-themes-nuanced-green)
                  ("plantuml" modus-themes-nuanced-green)
                  ("xml" modus-themes-nuanced-green)

                  ("css" modus-themes-nuanced-red)
                  ("scss" modus-themes-nuanced-red)
                  ("sql" modus-themes-nuanced-red)

                  ("conf" modus-themes-nuanced-cyan)
                  ("docker" modus-themes-nuanced-cyan)
                  ("json" modus-themes-nuanced-cyan)
                  ("makefile" modus-themes-nuanced-cyan)
                  ("yaml" modus-themes-nuanced-cyan)))
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

          (customize-set-variable 'highlight-symbol-colors `(,bg-yellow-intense
                                                             ,bg-magenta-intense
                                                             ,bg-cyan-intense
                                                             ,bg-green-intense
                                                             ,bg-red-intense
                                                             ,bg-blue-intense))

          (custom-theme-set-faces
           'user
           `(fixed-pitch ((t (,@c :family ,(face-attribute 'default :family) :height ,(face-attribute 'default :height)))))
           `(scroll-bar ((t (,@c :background ,bg-inactive :foreground ,fg-dim))))
           `(fill-column-indicator ((t (,@c :height 1.0 :background ,bg-main :foreground ,bg-inactive))))
           `(exordium-org-work ((t (,@c :inherit 'org-todo :foreground ,rust))))
           `(exordium-org-wait ((t (,@c :inherit 'org-todo :foreground ,cyan))))
           `(exordium-org-stop ((t (,@c :inherit 'org-todo :foreground ,fg-dim))))
           `(iedit-occurrence
             ((t (,@c :inherit nil
                      :box (:line-width -2 ,@(when-let* ((color (face-attribute 'modus-themes-completion-match-0 :foreground))
                                                         ((not (eq color 'unspecified))))
                                               (list :color color)))))))
           `(iedit-read-only-occurrence
             ((t (,@c :inherit nil
                      :box (:line-width -2 ,@(when-let* ((color (face-attribute 'modus-themes-completion-match-1 :foreground))
                                                         ((not (eq color 'unspecified))))
                                               (list :color color)))))))
           `(highlight-symbol-face ((t (,@c :background ,bg-cyan-nuanced))))
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
           `(helm-buffer-archive ((t (,@c :foreground ,warning))))
           `(helm-buffer-directory ((t (,@c :inherit dired-directory))))
           `(helm-buffer-file ((t (,@c :foreground ,fg-main))))
           `(helm-buffer-modified ((t (,@c :foreground ,yellow-warmer))))
           `(helm-buffer-not-saved ((t (,@c :foreground ,red-warmer))))
           `(helm-buffer-process ((t (,@c :foreground ,accent-1))))
           `(helm-buffer-saved-out ((t (,@c :inherit bold :background ,bg-cyan-nuanced :foreground ,red))))
           `(helm-buffer-size ((t (,@c :inherit shadow))))
           `(helm-ff-backup-file ((t (,@c :inherit shadow))))
           `(helm-ff-denied ((t (,@c :inherit dired-warning))))
           `(helm-ff-directory ((t (,@c :inherit dired-directory))))
           `(helm-ff-dirs ((t (,@c :inherit bold :foreground ,blue-cooler))))
           `(helm-ff-dotted-directory ((t (,@c :inherit (dired-header dired-directory)))))
           `(helm-ff-dotted-symlink-directory ((t (,@c :inherit (dired-symlink dired-directory)))))
           `(helm-ff-executable ((t (,@c :foreground ,accent-1))))
           `(helm-ff-file ((t (,@c :foreground ,fg-main))))
           `(helm-ff-file-extension ((t (,@c :foreground ,variable))))
           `(helm-ff-invalid-symlink ((t (,@c :inherit dired-broken-symlink))))
           `(helm-ff-suid ((t (,@c :inherit dired-set-id))))
           `(helm-ff-symlink ((t (,@c :inherit dired-symlink))))
           `(helm-ff-pipe ((t (,@c :inherit dired-special))))
           `(helm-ff-socket ((t (,@c :inherit dired-special))))
           `(helm-ff-truename ((t (,@c :foreground ,fg-main))))
           `(helm-ff-prefix ((t (,@c :background ,bg-ochre :foreground ,magenta))))
           `(helm-history-deleted ((t (,@c :inherit shadow :strike-through t))))
           `(helm-history-remote ((t (,@c :background ,bg-ochre))))
           `(helm-delete-async-message ((t (,@c :inherit bold :foreground ,magenta))))
           `(helm-ff-rsync-progress ((t (,@c :inherit bold :foreground ,red-warmer))))
           `(helm-rg-match-text-face ((t (,@c :inherit modus-themes-completion-match-0))))
           `(helm-rg-line-number-match-face ((t (,@c :inherit helm-grep-lineno))))
           `(helm-rg-file-match-face ((t (,@c :inherit helm-moccur-buffer))))
           `(helm-rg-preview-line-highlight ((t (,@c :inherit highlight :extend t))))
           `(helm-rg-title-face ((t (,@c :inherit helm-source-header))))
           `(helm-rg-base-rg-cmd-face ((t (,@c :inherit helm-source-header))))
           `(helm-rg-active-arg-face ((t (,@c :foreground ,yellow-warmer))))
           `(helm-rg-inactive-arg-face ((t (,@c :foreground ,fg-dim :weight thin))))
           `(helm-rg-extra-arg-face ((t (,@c :foreground ,yellow-cooler))))
           `(helm-rg-directory-cmd-face ((t (,@c :inherit helm-ff-directory))))
           `(helm-rg-directory-header-face ((t (,@c :inherit helm-ff-directory))))
           `(helm-M-x-key ((t (,@c :inherit modus-themes-key-binding))))
           `(helm-M-x-short-doc ((t (,@c :inherit completions-annotations)))))))))

  (defun pk/modus-themes--on-appearance-change ()
    "Switch theme according to current system setting."
    (when-let* ((appearance (plist-get (mac-application-state) :appearance))
                (desired-theme (let ((case-fold-search t))
                                 (if (string-match-p "dark" appearance)
                                     (cadr modus-themes-to-toggle)
                                   (car modus-themes-to-toggle))))
                ((not (eq desired-theme (car custom-enabled-themes)))))
      (message "Loading theme: %s" desired-theme)
      (modus-themes-load-theme desired-theme)))

  :config
  (when (boundp 'mac-effective-appearance-change-hook)
    (add-hook 'mac-effective-appearance-change-hook
              #'pk/modus-themes--on-appearance-change))

  (when-let* (((fboundp 'mac-application-state))
              (appearance (plist-get (mac-application-state) :appearance))
              (desired-theme (let ((case-fold-search t))
                               (if (string-match-p "dark" appearance)
                                   (cadr modus-themes-to-toggle)
                                 (car modus-themes-to-toggle)))))
    (message "Loading theme: %s" desired-theme)
    (modus-themes-load-theme desired-theme))

  (pk/modus-themes--custom-faces)

  :hook
  (modus-themes-after-load-theme . pk/modus-themes--custom-faces)

  :bind
  ("<f5>" . modus-themes-toggle))



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
  (setq mac-frame-tabbing nil)
  (defvar find-function-C-source-directory)
  (setq find-function-C-source-directory
        (when-let* ((emacs-src-dir
                     (file-name-concat (getenv "HOME") "gh" "mituharu" "emacs-mac" "src"))
                    ((file-exists-p emacs-src-dir)))
          emacs-src-dir))
  ;; Workaround to prevent freezes when frame is resized during startup
  ;; in Emacs-29.4 on macOS 15 Sequoia
  (add-hook 'kill-emacs-hook #'(lambda ()
                                 (dolist (frame (frame-list))
                                   (when (eq 'fullscreen
                                             (frame-parameter frame 'fullscreen))
                                     (set-frame-parameter frame 'fullscreen nil))))))

(defun pk/iterm-cut-base64 (text)
  "Take TEXT and send it to iTerm2 to copy."
  (interactive)
  (let ((base-64 (base64-encode-string text :no-line-break)))
    (send-string-to-terminal (concat "\e]1337;Copy=:" base-64 "\a"))))

(defun pk/dispatch-cut-function (orig-fun text)
                                        ; checkdoc-params: (orig-fun)
  "Dispatch the TEXT to the appropriate `interprogram-cut-function'."
  (if (display-graphic-p)
      (funcall orig-fun text)
    (pk/iterm-cut-base64 text)))

(advice-add interprogram-cut-function :around #'pk/dispatch-cut-function)


;; https://ylluminarious.github.io/2019/05/23/how-to-fix-the-emacs-mac-port-for-multi-tty-access/
(use-package mac-pseudo-daemon
  :hook
  ((after-init . mac-pseudo-daemon-mode)
   (after-init . server-start)))

(setq disabled-command-function nil)
(setq confirm-kill-emacs 'y-or-n-p)
(setq-default display-line-numbers-widen t)



(use-package window
  :ensure nil
  :custom
  ;; Typical full screen window on built in macBook Air M2 13" is 179
  ;; columns. This includes line numbers and fringes.  Yet windows as small as
  ;; 160 seems to be quite all right fitting horizontal split.  On the other
  ;; hand, typical full screen window height on external 4k screen connected to
  ;; the same laptop is 88. See `split-window-sensibly' doc.  This setup
  ;; basically forces a horizontal split, but only up to 2 windows in a frame.
  ;; Values used are compared against:
  ;;
  ;; (window-width (selected-window))
  ;; (window-height (selected-window))
  ;;
  ;; For testing use:
  ;;
  ;; (split-window-sensibly)
  (split-height-threshold 90)
  (split-width-threshold 160))


;; ITERM2 MOUSE SUPPORT from https://www.emacswiki.org/emacs/iTerm2
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (_)))

(defconst pk/shrug-string "¯\\_(ツ)_/¯")

(defun pk/shrug ()
  "Insert ¯\\_(ツ)_/¯ at point."
  (interactive)
  (insert (if (derived-mode-p 'markdown-mode)
              (replace-regexp-in-string (rx (group (or "\\" "_")))
                                        (rx "\\" (backref 1))
                                        pk/shrug-string)
            pk/shrug-string)))

(defun pk/shrug-as-kill ()
  "Add ¯\\_(ツ)_/¯ to kill buffer."
  (interactive)
  (kill-new pk/shrug-string))

;; @todo remove when exordium has it
;; (use-package helm
;;   :diminish
;;   :custom
;;   (helm-split-window-other-side-when-one-window 'right)
;;   (helm-M-x-show-short-doc t)
;;   :bind
;;   (("C-x b" . #'helm-buffers-list)
;;    :map ctl-x-map
;;         ("b" . #'helm-buffers-list)
;;    :map helm-command-map
;;         ("g" . #'helm-google-suggest)))


;; @todo : remove when exordium has it
;; (use-package elisp-mode
;;   :ensure nil
;;   :bind
;;   (:map emacs-lisp-mode-map
;;         ("M-." . #'xref-find-definitions)
;;         ("M-," . #'xref-pop-marker-stack)
;;         ("M-r" . #'xref-find-references)
;;         ("M-?" . #'helpful-at-point)))

(use-package eldoc
  :ensure nil
  :diminish)

(use-package flycheck
  :autoload (flycheck-add-next-checker
             flycheck-sanitize-errors
             flycheck-dedent-error-messages
             flycheck-rx-to-string
             flycheck-buffer-saved-p
             flycheck-checker-get)
  :functions (pk/loaddefs-generate--filter-flycheck)
  :init
  (use-package project
    :ensure nil
    :autoload (project-root))

  (defun pk/loaddefs-generate--filter-flycheck (args)
    "Append flycheck_ files to ignored files in 2nd arg in ARGS.
When such temporary flycheck_ files are present they can be
scrubbed for autoloads clobbering real declarations.  This only
happens in `project' directories, such that it shouldn't kick in
packages are reinstalled from MELPA."
    (when-let* ((default-directory (nth 0 args))
                (project-root (project-root (project-current))))
      (setf (nth 2 args)
            (append (nth 2 args)
                    (directory-files
                     project-root t
                     (rx string-start "flycheck_"
                         (one-or-more (or alnum punct)) ".el" string-end)))))
    args)

  :config
  (advice-add 'loaddefs-generate
              :filter-args #'pk/loaddefs-generate--filter-flycheck)

  (flycheck-define-checker pk/python-blocklint
    "Blocklint: blocks usage of non-inclusive wording.
See: https://github.com/PrincetonUniversity/blocklint"
    :command ("blocklint" "--max-issue-threshold" "1" source-inplace)
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
    :modes (python-mode python-ts-mode))

  (add-to-list 'flycheck-checkers 'pk/python-blocklint 'append)
  (mapc (lambda (checker)
          (flycheck-add-next-checker checker '(warning . pk/python-blocklint) t))
        '(python-flake8 python-pylint python-pycompile))

  (flycheck-define-checker pk/sorbet-homebrew
    "Sorbet for Homebrew"
    :command ("bundle" "exec" "srb" "tc"
              "--color" "never"
              "--no-error-count"
              "--no-error-sections"
              "--suppress-error-code" "1001"
              "--suppress-error-code" "1003")
    :working-directory
    (lambda (_)
      (when-let* ((homebrew-prefix (getenv "HOMEBREW_PREFIX")))
        (file-name-concat homebrew-prefix "Library" "Homebrew")))
    :predicate (lambda () (flycheck-buffer-saved-p))
    :error-patterns
    ((error
      line-start (file-name) ":" (optional line ": ")
      (message (one-or-more not-newline)
               " https://srb.help/" (id (one-or-more digit)) "\n"
               (optional
                (one-or-more " ") (one-or-more digit) " |"
                (one-or-more " ") (one-or-more not-newline) "\n"
                (one-or-more " ") (one-or-more "^") (one-or-more "\n")))
      line-end))
    :error-filter
    (lambda (errors)
      (flycheck-sanitize-errors (flycheck-dedent-error-messages errors)))
    :error-explainer
    (lambda (err)
      (let ((error-code (flycheck-error-id err))
            (url "https://srb.help/%s"))
        (and error-code `(url . ,(format url error-code)))))
    :modes (ruby-mode ruby-ts-mode enh-ruby-mode)
    :enabled
    (lambda ()
      (when-let* ((buffer-file-name)
                  (default-directory (file-name-directory buffer-file-name))
                  (homebrew-prefix (getenv "HOMEBREW_PREFIX"))
                  (project-current (project-current))
                  ((string-match-p (rx-to-string `(seq string-start ,homebrew-prefix) t)
                                   (project-root (project-current)))))
        ;; Ensure sorbet and gems are up to date, by running "brew typecheck"
        (let ((display-buffer-alist '((t . (display-buffer-no-window)))))
          (async-shell-command "brew typecheck"))
        t)))

  (add-to-list 'flycheck-checkers 'pk/sorbet-homebrew 'append)
  (flycheck-add-next-checker 'ruby '(warning . pk/sorbet-homebrew) t)

  ;; Fix for https://github.com/flycheck/flycheck/issues/2092
  (let* ((p '(error line-start (file-name) ":"
                   (zero-or-more whitespace) "Error:" (zero-or-more whitespace)
                   (message (or "End of file during parsing"
                                (seq "Invalid read syntax:" (zero-or-more (not ",")))))
                   (optional "," (zero-or-more whitespace) line
                             "," (zero-or-more whitespace) column)))
         ;; pre-compile pattern like `flycheck-define-command-checker' does
         (pattern (cons (flycheck-rx-to-string `(and ,@(cdr p))
                                               'no-group)
                        (car p))))
    (unless (cl-find-if (lambda (p)
                          (and (eq (cdr p) (cdr pattern))
                               (equal (car p) (car pattern))))
                        (flycheck-checker-get 'emacs-lisp 'error-patterns))
      (push pattern (flycheck-checker-get 'emacs-lisp 'error-patterns)))))


(use-package clang-format
  :defer t
  :config
  (use-package cc-mode :ensure nil
    :bind
    (:map c-mode-map
          ("C-c C-f" . #'clang-format-region)
     :map c++-mode-map
          ("C-c C-f" . #'clang-format-region)))
  (use-package c-ts-mode :ensure nil
    :bind
    (:map c-ts-mode-map
          ("C-c C-f" . #'clang-format-region)
     :map c++-ts-mode-map
          ("C-c C-f" . #'clang-format-region))))
;;@todo: disable printing from eglot - perhaps set `eglot-events-buffer-size' 0
(use-package eglot
  :exordium-force-elpa "gnu"
  :after flycheck
  :init
  (defun pk/eglot--disable-standard-c/c++-checkers ()
    (mapc
     (lambda (checker)
       (if eglot--managed-mode
           (add-to-list 'flycheck-disabled-checkers checker)
         (delete checker flycheck-disabled-checkers)))
     '(c/c++-clang c/c++-gcc)))

  (global-unset-key (kbd "C-c e"))
  (global-unset-key (kbd "C-c E"))

  :hook
  (eglot-managed-mode . pk/eglot--disable-standard-c/c++-checkers)
  :custom
  (eglot-extend-to-xref t)

  :bind
  (("C-c e e" . eglot)
   :map eglot-mode-map
   ("C-c e r" . eglot-rename)
   ("C-c e a" . eglot-code-actions)
   ("C-c e q" . eglot-shutdown)
   ("C-c e ?" . eldoc)
   ("C-c e h" . eldoc)
   ("C-c e L" . helm-flycheck)
   ("C-c e l" . flycheck-list-errors)
   ("C-c e C" . eglot-show-workspace-configuration)
   ("C-c e S" . eglot-signal-didChangeConfiguration)
   ("C-c e x r" . eglot-reconnect)
   ("C-c e x l" . eglot-list-connections)
   ("C-c e x E" . eglot-stderr-buffer)
   ("C-c e x e" . eglot-events-buffer)))

  (use-package flycheck-eglot
    :after (flycheck eglot)
    :defer t
    :custom
    (flycheck-eglot-exclusive nil)
    :config
    (global-flycheck-eglot-mode 1))


(use-package eldoc
  :ensure nil
  :defer t
  :custom
  (eldoc-idle-delay 0.25)
  (eldoc-echo-area-prefer-doc-buffer t))

(use-package dumb-jump
  :defer t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


;; @todo: remove when exordium has it
;; (use-package orderless
;;   :custom
;;   (helm-completion-style 'emacs)
;;   (completion-styles '(orderless basic))
;;   (completion-category-overrides '((file (styles basic partial-completion)))))


(use-package ispell
  ;;   :ensure-system-package aspell
  :defer t
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
  :defer t
  :config
  (setq flyspell-issue-message-flag nil)
  :hook
  ((git-commit-mode . flyspell-mode)
   (org-mode        . flyspell-mode)
   (text-mode       . flyspell-mode))
  :bind
  (([remap ispell-word] . #'flyspell-correct-wrapper)))

(use-package flyspell-correct-helm
  :after (flyspell flyspell-correct)
  :defer t
  :autoload (flyspell-correct-helm)
  :custom
  (flyspell-correct-interface #'flyspell-correct-helm))



(set-time-zone-rule "/usr/share/zoneinfo/Europe/London")



(use-package groovy-mode
  :init
  (defun pk/groovy-mode--create-test-files ()
    (setq-local projectile-create-missing-test-files t))
  :hook
  ((groovy-mode . yas-minor-mode)
   (groovy-mode . pk/groovy-mode--create-test-files)))

(use-package jenkinsfile-mode
  :after (groovy-mode)
  :init
  (use-package flycheck
    :autoload (flycheck-add-mode))
  :defer t
  :config
  (flycheck-add-mode 'groovy 'jenkinsfile-mode)
  (setq auto-mode-alist
        (cl-remove-if (lambda (entry)
                        (and (string-match-p "Jenkinsfile" (car entry))
                             (eq 'groovy-mode (cdr entry))))
                      auto-mode-alist)))

(use-package yasnippet
  :defines (yas-snippet-dirs)
  :defer t
  :config
  (when-let* ((file-name (or load-file-name
                             (buffer-file-name)))
              (snippets-directory (file-name-concat
                                   (file-name-directory file-name)
                                   "snippets"))
              ((file-directory-p snippets-directory)))
    (customize-set-variable 'yas-snippet-dirs
                            (cons snippets-directory yas-snippet-dirs)
                            "Customized with use-package yasnippet")))


(use-package projectile
  :autoload (projectile-project-name
             projectile-register-project-type
             projectile-verify-file
             projectile-verify-file-wildcard)
  :functions (pk/projectile--emacs-package-build
              pk/projectile--emacs-package-project-p)
  :init
  (defun pk/projectile--emacs-package-project-p (&optional dir)
    (or (projectile-verify-file "Cask" dir)
        (projectile-verify-file "init.el" dir) ;; Exordium!
        (projectile-verify-file "lisp" dir)
        (and (projectile-verify-file "src" dir)
             (projectile-verify-file-wildcard "src/*.el" dir))))
  (defun pk/projectile--emacs-package-build ()
    (when-let* ((project-name (projectile-project-name))
                (pkg-desc (cadr
                           (cl-find-if
                            (lambda (package)
                              (string-match-p
                               (rx string-start
                                   (literal (symbol-name (car package)))
                                   (zero-or-one ".el")
                                   string-end)
                               project-name))
                            package-alist))))
      (package-vc-rebuild pkg-desc)))
  (defun pk/projectile--emacs-package-test ()
    (call-interactively #'ert))
  :config
  (projectile-register-project-type
   'pk/emacs-package
   #'pk/projectile--emacs-package-project-p
   :install #'pk/projectile--emacs-package-build
   :test "make compile lint test"
   :test-dir "test/"
   :test-suffix ".t"))


(use-package yaml-mode
  :defer t
  :mode (rx ".y" (zero-or-one "a") "ml.template" string-end))

(use-package rust-mode
  :defer t)

(use-package compile
  :defer t
  :ensure nil
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :custom
  (ansi-color-for-compilation-mode t)
  (compilation-scroll-output 'first-error))


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


;; (use-package posframe
;;   :defer t)
;; (use-package ace-window
;;   :defer t
;;   :after (posframe)
;;   :diminish "AW"
;;   :custom
;;   (aw-scope 'frame)
;;   (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;;   (aw-translate-char-function #'(lambda (c)
;;                                   (if (eq ?\M-o c) ?n c)))
;;   :config
;;   (global-set-key (kbd "M-o") #'ace-window)
;;   (when (and (require 'posframe nil t) (posframe-workable-p))
;;     (ace-window-posframe-mode)))


(defconst pk/desktop-files-not-to-save
  (rx-let ((path (+ (or alnum "." "/" "-" "_" "~"))))
    (rx (or (seq string-start "/" (zero-or-more (not (any "/" ":"))) ":")
            (seq "(ftp)" string-end)
            (seq string-start path "/emacs/" path "/lisp/" path
                 ".el.gz" string-end)
            (seq string-start path "/.emacs.d/elpa"
                 (zero-or-one "-" (one-or-more digit) "." (one-or-more alnum))
                 "/" path
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
  (add-to-list 'desktop-modes-not-to-save 'magit-status-mode)
  (add-to-list 'desktop-modes-not-to-save 'magit-process-mode)
  (add-to-list 'desktop-modes-not-to-save 'magit-diff-mode)
  (add-to-list 'desktop-modes-not-to-save 'forge-pullreq-mode)
  (add-to-list 'desktop-modes-not-to-save 'forge-notifications-mode)
  (add-to-list 'desktop-modes-not-to-save 'difftastic-mode)
  :custom
  (desktop-files-not-to-save pk/desktop-files-not-to-save)
  (desktop-restore-eager 8)
  (desktop-load-locked-desktop (if (version< "29" emacs-version) 'check-pid 'ask)))


(use-package savehist
  :ensure nil
  :config
  (add-to-list 'savehist-additional-variables 'command-history)
  (add-to-list 'savehist-additional-variables 'extended-command-history)
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (add-to-list 'savehist-additional-variables 'search-ring)
  (add-to-list 'savehist-additional-variables 'regexp-search-ring)
  (add-to-list 'savehist-additional-variables 'register-alist)
  (add-to-list 'savehist-additional-variables 'compile-command)
  (add-to-list 'savehist-additional-variables 'compile-history)
  (savehist-mode))

(use-package python
  :ensure nil
  :init
  (use-package treesit
    :ensure nil
    :autoload (treesit-node-text))
  (defun pk/python-mode--set-fill-column()
    (when-let* ((project-root (project-root (project-current)))
                ;; do not override directory local variable
                ((not
                  (alist-get
                   'fill-column
                   (alist-get
                    major-mode
                    (alist-get
                     (car (alist-get
                           (file-name-as-directory project-root)
                           dir-locals-directory-cache nil nil #'string=))
                     dir-locals-class-alist)))))
                (pyproject-toml (expand-file-name "pyproject.toml" project-root))
                ((file-exists-p pyproject-toml))
                (buffer (current-buffer)))
      (with-temp-buffer
        (insert-file-contents pyproject-toml)
        (when-let* ((line-length-node
                     (alist-get
                      'line-length
                      (treesit-query-capture
                       'toml
                       '(((table
                           (dotted_key) @table-name
                           (pair (bare_key) @key "=" (integer) @line-length))
                          (:match "tool\\.\\(ruff\\|black\\)" @table-name)
                          (:equal "line-length" @key))))))
                    (line-length (string-to-number
                                  (treesit-node-text line-length-node))))
          (with-current-buffer buffer
            (setq fill-column line-length))))))
  :hook
  (python-ts-mode . pk/python-mode--set-fill-column)
  :custom
  (python-shell-dedicated 'project)
  (python-shell-interpreter  "python3"))

(use-package toml)

(use-package python-pytest
  :after (python)
  :bind (:map python-mode-map
              ("C-c t" . python-pytest-dispatch))
  :custom
  (python-pytest-executable
   (concat python-shell-interpreter " -m pytest")))

(use-package pip-requirements)

;; See https://blog.adam-uhlir.me/python-virtual-environments-made-super-easy-with-direnv-307611c3a49a
;; for layout thing
(use-package direnv
  ;;   :ensure-system-package direnv
  :config
  (direnv-mode))

(use-package compile
  :ensure nil
  :config
  ;; To ignore: PipDeprecationWarning: DEPRECATION:
  ;; file:///.#egg=package.name>=0.dev contains an egg fragment with a non-PEP
  ;; 508 name pip 25.0 will enforce this behaviour change. A possible
  ;; replacement is to use the req @ url syntax, and remove the egg
  ;; fragment. Discussion can be found at
  ;; https://github.com/pypa/pip/issues/11617
  (add-to-list
   'compilation-transform-file-match-alist
   '("/.*/lib/python[0-9\\.]+/site-packages/pip/_internal/models/link.py\\'" nil)))


;; Diminish some modes
(diminish 'eldoc-mode)
(diminish 'auto-revert-mode)
(diminish 'undo-tree-mode)
(diminish 'git-gutter-mode)


(use-package swiper
  :init
  (use-package ivy
    :autoload (ivy-previous-history-element
               ivy-previous-line
               ivy-exit-with-action)
    :init
    (defun pk/swiper-C-r (&optional arg)
      "Move cursor vertically up ARG candidates.
If the input is empty, select the previous history element instead."
      (interactive "p")
      (if (string= ivy-text "")
          (ivy-previous-history-element 1)
        (ivy-previous-line arg))))

  (use-package iedit
    :autoload (iedit-lib-cleanup
               iedit-start
               iedit-done)
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
  (when (fboundp 'call-interactively@ido-cr+-record-current-command)
    (advice-remove 'call-interactively #'call-interactively@ido-cr+-record-current-command))
  (when (fboundp 'ido-minibuffer-setup)
    (remove-hook 'minibuffer-setup-hook #'ido-minibuffer-setup)))

(use-package ob-python
  :ensure nil
  :custom
  (org-babel-python-command "python3"))

(use-package org
  :ensure nil
  :custom
  (org-src-tab-acts-natively t))

;; (use-package magit-todos
;;   :ensure-system-package (rg . ripgrep)
;;   :config
;;   (magit-todos-mode))

;; (define-advice
;;     magit-remote--cleanup-push-variables
;;     (:after (old &optional new)
;;             pk/magit-remove--cleanup-push-variables--cleanup-forge-remote)
;;   "cleanup local forge.remote variable to follow the `new' remote.
;; preferably the local value is used, but if not defined then fallback to global.
;; the local value is removed when it was pointing to a removed remote or it is
;; the same as global. in detail, the pseudo-code is:
;; - rename
;;  - have global
;;   - global matches old
;;    - set local to new
;;  - have local
;;   - local matches old
;;    - set local to new
;;  - have global and local
;;   - local matches old and global doesn't match new
;;    - set local to new
;;   - local matches old and global matches new
;;    - remove local
;;  - old matches \"origin\"
;;   - local is nil and global doesn't match new
;;    - set local to new
;; - remove
;;  - have local
;;   - local matches old
;;    - remove local
;;  - have global and local
;;   - local matches old
;;    - remove local"
;;   (let ((global (when new (magit-get "--global" "forge.remote")))
;;         (local (magit-get "--local" "forge.remote")))
;;     (if new
;;                                         ; rename
;;         (cond
;;          ((and local global)
;;           (when (string= local old)
;;             (if (string= global new)
;;                 (magit-set nil "--local" "forge.remote")
;;               (magit-set new "--local" "forge.remote"))))
;;          ((or (and local (string= local old))
;;               (and global (string= global old))
;;               (and (not local)
;;                    (and global (not (string= global new)))
;;                    (string= "origin" old)))
;;           (magit-set new "--local" "forge.remote")))
;;                                         ; remove
;;       (when (and local (string= local old))
;;         (magit-set nil "--local" "forge.remote")))))



;; Load R as well
(use-package ess
  :defer t
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
  :defer t
  :ensure nil
  :functions (pk/url-netrc-auth)
  :init
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
  :config
  (url-register-auth-scheme "netrc" #'pk/url-netrc-auth 1))

(use-package restclient
  :defer t)
(use-package restclient-helm
  :defer t)
(use-package ob-restclient
  :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '((restclient . t)))))

(use-package company-restclient
  :defer t
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package org-tree-slide
  :after org
  :defer t
  :bind
  (:map org-mode-map
        ("<f8>" . org-tree-slide-mode)
        ("S-<f8>". org-tree-slide-skip-done-toggle)))


(require 'map)
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
  :type '(repeat string)
  :group 'pk/exordium)
(when (fboundp 'mac-auto-operator-composition-shape-gstring)
  (defvar pk/mac-auto-operator-composition-mode)
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
      (clrhash mac-auto-operator-composition-cache))))
(when (fboundp 'pk/mac-auto-operator-composition-mode)
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

(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain))

;; TODO: move to exordium and likely hide behind the
;; `exordium-use-variable-pitch' and `exordium-complete-mode' set to `:company'
(use-package company-posframe
  :after (posframe)
  :autoload (company-posframe-quickhelp-toggle
             company-select-next
             company-select-previous)
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

When TYPE is \\='first and not in `tab-bar-history-mode' or TYPE
is \\='first-history the leading space will be omitted.  When
type is LAST the trailing space will be omitted.

When CURRENT is \\='this then trailing space and the ?¦ will have
\\='tab-bar-tab face property.  When CURRENT is \\='previous then
leading space and the ?¦ will have \\='tab-bar-tab face property.
All the reminder parts of the separator will have
\\='tab-bar-tab-inactive face property."
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
       (when (and (memq 'tab-bar-format-add-tab tab-bar-format) tab-bar-new-button)
         `((add-tab menu-item ,tab-bar-new-button tab-bar-new-tab
                    :help "New tab"))))))

  :bind
  (("M-<tab>" . tab-next)
   ("M-S-<tab>" . tab-previous)
   ("H-t" . tab-new)))



;; evil likes to turn itself on, let's disable it so no surprises
;; (use-package evil
;;   :init
;;   (defun pk/disable-evil-mode ()
;;     (unless exordium-enable-evil-mode
;;       (when (or evil-mode evil-local-mode)
;;         (turn-off-evil-mode))))
;;   :hook
;;   ((evil-mode . pk/disable-evil-mode)
;;    (evil-local-mode . pk/disable-evil-mode))
;;   :config
;;   (advice-remove #'select-window #'ad-Advice-select-window))


(use-package so-long
  :init
  (defun pk/so-long-skip-bidi-inhibit-bpa-override ()
    "Don't override `bidi-inhibit-bpa' by `so-long'."
    (setq-local so-long-variable-overrides
                (remove '(bidi-inhibit-bpa . t) so-long-variable-overrides)))
  (use-package nxml-mode
    :ensure nil
    :defer t
    :hook
    (nxml-mode . #'pk/so-long-skip-bidi-inhibit-bpa-override))
  :config
  (setq-default bidi-paragraph-direction 'left-to-right)
  (global-so-long-mode))


(use-package git-link
  :after (magit transient)
  :custom
  (git-link-use-commit t)
  :config
  (transient-append-suffix 'magit-file-dispatch "g" '(1 "f" "copy link" git-link)))


(use-package dom
  :ensure nil
  :autoload (dom-text
             dom-by-tag))

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
  :autoload (org-combine-plists
             org-in-regexp)
  :init
  (defun pk/orgtbl-to-gfm (table params)
    "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
    (let* ((hline (concat (mapconcat (lambda (x)
                                       (pcase x
                                         ("r" "|--:")
                                         ("c" "|:-:")
                                         ("l" "|:--")
                                         (_  "|---")))
                                     org-table-last-alignment "")
                          "|")))
      (orgtbl-to-generic table (org-combine-plists
                                (list :splice t
	                                  :hline hline
                                      :lstart "| " :lend " |" :sep " | ")
                                params))))
  :bind
  (:map org-mode-map
   ([remap org-insert-link] . #'pk/org-insert-link-dwim)))

(use-package markdown-mode
  :custom
  ;; Use `github-markup' for makrdown with a github style
  ;; This requires `github-markup' gem to be installed, i.e.:
  ;; $ sudo gem install github-markup
  (markdown-command "github-markup")
  (markdown-command-needs-filename t)
  (markdown-css-paths (list (concat
                             "file://"
                             (file-name-directory
                              (or load-file-name buffer-file-name))
                             "github-markdown.css")))
  (markdown-xhtml-body-preamble "<article class=\"markdown-body\">")
  (markdown-xhtml-body-footer "</article>")
  :bind
  (:map markdown-mode-map
   ([remap markdown-insert-link] . #'pk/markdown-insert-link-dwim)))


(use-package mode-line-bell
  :config
  (mode-line-bell-mode))

(use-package dired
  :ensure nil
  :custom
  (dired-chown-program (or (executable-find "gchown") "chown"))
  (dired-touch-program (or (executable-find "gtouch") "touch"))
  (dired-use-ls-dired 'unspecified)
  (dired-dwim-target t)
  :config
  (setq insert-directory-program (or (executable-find "gls") "ls")))

(use-package dired-du
  :custom
  (dired-du-used-space-program `(,(or (executable-find "gdu") "du") "-sb")))

(use-package ob-async
  :defer t
  :after org)

(use-package transpose-frame)

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package protobuf-mode)

(use-package apheleia
  :after (project)
  :init
  (defun pk/apheleia-update-python-formatters ()
    "Turn on `apheleia-mode' for python mode with formatters as per lint.in."
    (interactive)
    (when-let* (((and (or (derived-mode-p 'python-ts-mode)
                          (derived-mode-p 'python-mode))))
                (project-root (project-root (project-current)))
                (lint-in
                 (seq-find
                  #'file-exists-p
                  (mapcar (lambda (elt)
                            (apply #'file-name-concat (cons project-root elt)))
                          '(("requirements-dev" "lint.in")
                            ("requirements" "lint.in")
                            ("requirements-dev.txt")))))
                (buffer (current-buffer)))
      (with-temp-buffer
        (insert-file-contents lint-in)
        (let ((formatters (mapcar
                           (lambda (elt)
                             (goto-char (point-min))
                             (if (re-search-forward (format "^\\W*%s\\b" (car elt)) nil t)
                                 (cadr elt)
                               (caddr elt)))
                           '(("black" black ruff-format)
                             ("isort" isort ruff-isort)))))
          (with-current-buffer buffer
            (setq apheleia-formatter formatters)
            (apheleia-mode))))))

  :hook
  (python-ts-mode . pk/apheleia-update-python-formatters)
  :custom
  (apheleia-formatters-respect-fill-column t)
  :config
  (setf (alist-get 'isort apheleia-formatters)
        '("isort" "--profile" "black" (apheleia-formatters-fill-column "--line-length") "-"))
  (setf (alist-get 'ruff-isort apheleia-formatters)
        '("ruff" "check" "--select" "I" "--exit-zero" "--fix" "--stdin-filename" filepath "-"))
  (setf (alist-get 'ruff-format apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath "-"))
  (setf (alist-get 'shfmt-homebrew apheleia-formatters)
        ;; from ${HOMEBREW_PREFIX}/.vscode/settings.json
        '((file-name-concat (or (getenv "HOMEBREW_LIBRARY")
                                "/opt/homebrew/Library")
                            "Homebrew" "utils" "shfmt.sh")
          "-filename" filepath
          "-ln" "bash"
          "-i" "2"
          "-ci"
          "-"))
  (setf (alist-get 'rubocop-homebrew apheleia-formatters)
        ;; too slow (append '("brew") (alist-get 'rubocop apheleia-formatters))
        (append '("ruby"
                  (file-name-concat (or (getenv "HOMEBREW_LIBRARY")
                                        "/opt/homebrew/Library")
                                    "Homebrew" "utils" "rubocop.rb"))
                (cdr (alist-get 'rubocop apheleia-formatters))))

  (setf (alist-get 'json-mode apheleia-mode-alist) 'jq)
  (setf (alist-get 'js-json-mode apheleia-mode-alist) 'jq)
  (setf (alist-get 'json-ts-mode apheleia-mode-alist) 'jq)

  (setf (alist-get 'ruby-mode apheleia-mode-alist) 'rubocop)
  (setf (alist-get 'ruby-ts-mode apheleia-mode-alist) 'rubocop)
  (setf (alist-get 'enh-ruby-mode apheleia-mode-alist) 'rubocop)

  (setf (alist-get 'sh-mode apheleia-mode-alist)
        (alist-get 'bash-ts-mode apheleia-mode-alist))

  (dolist (mode '(python-mode python-ts-mode))
    (setf (alist-get mode apheleia-mode-alist)
          '(ruff-format ruff-isort))))


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
  "Set `sql-product' according to `:engine'.
As extracted by `pk/sql--ob-fontify-init-engine'.

This is intended to be used as an advise for `org-font-lock-ensure'."
  (when (and org-src-fontify-natively
             (eq major-mode 'sql-mode))
    (sql-set-product (if pk/sql--ob-fontify-engine
                         (if-let* ((product (intern pk/sql--ob-fontify-engine))
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
  ;; Disable js2-mode that exordium turns on for `javascrip-mode'
  ;; to disable js-lint
  :defer t
  :hook
  (json-mode . (lambda ()
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


;; Inspired by Bart Spiers and Piotr Kazanowski's:
;; https://pkaznowski.gitlab.io/blog/post/sort-words-in-region/

(defun pk/sort-words-in-region (beg end &optional reversed)
                                        ; checkdoc-params: (beg end)
  "In active region sort comma separated strings in ascending order.

With prefix arg REVERSED sort in descending order."
  (interactive  "r\nP")
  (unless (region-active-p) (user-error "No active region to sort!"))
  (replace-region-contents
   beg end
   (lambda ()
     (string-join
      (sort
       (mapcar #'string-trim
               (string-split
                (buffer-substring-no-properties (point-min) (point-max))
                ","))
       (if reversed #'string> #'string<))
      ", "))))


;; Slightly modified version of https://www.emacswiki.org/emacs/AddCommasToNumbers

(defun pk/add-number-grouping (number &optional separator)
  "Add commas to NUMBER and return it as a string.
Optional SEPARATOR is the string to use to separate groups.  It
defaults to a comma."
  (let ((num (if (stringp number)
                 number
               (number-to-string number)))
        (op (or separator ",")))
    (save-match-data
      (while (string-match "\\(.*[0-9]\\)\\([0-9]\\{3\\}[0-9,\.]*\\)" num)
        (setq num (concat (match-string 1 num)
                          op
                          (match-string 2 num)))))
    num))


(defun pk/remove-number-grouping (number &optional separator)
  "Remove commas from NUMBER and return it as a number.
Optional SEPARATOR is the string to use to separate groups.  It
defaults to a comma."
  (let ((sep (or separator ?,)))
    (string-to-number (cl-remove sep number))))


(defun pk/mapcar-grouped-numbers (numbers &optional separator)
  "Remove commas from NUMBERS and return them a list of numbers.
Optional SEPARATOR is the string to use to separate groups.  It
defaults to a comma."
  (mapcar (lambda (num)
            (pk/remove-number-grouping num separator))
          numbers))


(use-package graphviz-dot-mode)

;; what variables should be updated has been copied from exec-path-from-shell-setenv
(defun pk/fixup-path-setenv ()
  "Fixup PATH variable: my bin, homebrew bin, reminder of the elements."
  (interactive)
  (let* ((home (getenv "HOME"))
         (brew-prefix (when-let* ((brew (executable-find "brew")))
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


;; (use-package flycheck-package
;;   :config
;;   (eval-after-load 'flycheck
;;     '(flycheck-package-setup)))


(use-package cperl-mode
  :ensure nil
  :mode (rx ".p" (or "l" "m") string-end)
  :interpreter "perl"
  :commands (cperl-set-style)
  :config
  (cperl-set-style "CPerl"))

(defcustom pk/dwim-shell-command-pip-no-binary nil
  "List of packages to pass to --no-binary pip flag."
  :type '(repeat (choice symbol string))
  :group 'pk/exordium)

(use-package dwim-shell-command
  :ensure t
  :demand t
  :autoload (dwim-shell-command-execute-script)
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command))

  :config
  (defun pk/dwim-shell-command-pip-upgrade-requirements()
    "Upgrade all requirements in current venv."
    (interactive)
    (when-let* ((default-directory (project-root (project-current)))
                ((or (getenv "VIRTUAL_ENV")
                     (user-error "No virtual environment is active")))
                (no-binary
                 (if pk/dwim-shell-command-pip-no-binary
                     (concat "--no-binary "
                             (mapconcat (lambda (package)
                                          (if (symbolp package)
                                              (symbol-name package)
                                            package))
                                        pk/dwim-shell-command-pip-no-binary
                                        ","))
                   "")))
      (dwim-shell-command-execute-script
       (format "pip upgrade<%s>" (project-name (project-current)))
       (format
        "pip --disable-pip-version-check list --outdated --format=json | \
           jq -r '.[] | .name' | \
           xargs -n1 pip install -U --prefer-binary %s"
        no-binary))))

  (defun pk/dwim-shell-command-pip-install-requirements ()
    "Pip install from requirements '*.in' or '*.txt' files."
    (interactive)
    (when-let* ((default-directory (project-root (project-current)))
                ((or (getenv "VIRTUAL_ENV")
                     (y-or-n-p
                      "No virtual environment is active.  Install requirements?")))
                (no-binary
                 (if pk/dwim-shell-command-pip-no-binary
                     (concat "--no-binary "
                             (mapconcat (lambda (package)
                                          (if (symbolp package)
                                              (symbol-name package)
                                            package))
                                        pk/dwim-shell-command-pip-no-binary
                                        ","))
                   "")))
      ;; when `default-directory' is used the `dwim-shell-command-execute-script'
      ;; jumps to the directory where it's been started
      (dwim-shell-command-execute-script
       (format "pip install -r <<**/requirements*.{in,txt}>><%s>"
               (project-name (project-current)))
       (format
        "have_in=
         for f in requirements{,-dev}.in requirements{,-dev}/{lint,misc,test}.in; do
           if [ -f \"${f}\" ]; then
             have_in=yes
             echo \"Installing requirements from ${f}\"
             pip install -r \"${f}\" --upgrade --prefer-binary %s
           fi
         done
         if [ -z ${have_in} ]; then
           for f in requirements{,-dev}.txt; do
             if [ -f \"${f}\" ]; then
               echo \"Installing requirements from ${f}\"
               pip install -r \"${f}\" --upgrade --prefer-binary %s
             fi
           done
         fi"
        no-binary no-binary)
       :error-autofocus t
       :silent-success t))))

(use-package dwim-shell-commands
  :ensure nil
  :demand t
  :after dwim-shell-command
  :commands dwim-shell-commands-kill-process)


(use-package shr
  :ensure nil
  :autoload (shr-render-buffer))

(defvar pk/ipynb--convert-command
  "jupyter nbconvert --to html --log-level WARN --stdout --stdin")

(defvar pk/ipynb--in-or-out-regexp
  (rx-to-string
   '(seq line-start (or "In" "Out") (zero-or-more space) "[" (one-or-more digit) "]:" line-end)))

(defun pk/ipynb--next-in-or-out ()
  "Find line beginning position of next In or Out."
  (save-excursion
    (goto-char (line-end-position))
    (when (re-search-forward pk/ipynb--in-or-out-regexp nil t)
      (line-beginning-position))))

(defun pk/ipynb--prev-in-or-out ()
  "Find line beginning position of previous In or Out."
  (save-excursion
    (goto-char (line-beginning-position))
    (backward-char)
    (cl-block searching-prev-chunk
      (when (re-search-backward pk/ipynb--in-or-out-regexp nil t)
        (line-beginning-position)))))

(defun pk/ipynb-next-in-or-out ()
  "Move to the next file."
  (interactive)
  (if-let* ((next (pk/ipynb--next-in-or-out)))
      (goto-char next)
    (user-error "No more In nor Out")))

(defun pk/ipynb-previous-in-or-out ()
  "Move to the previous file."
  (interactive)
  (if-let* ((previous (pk/ipynb--prev-in-or-out)))
      (goto-char previous)
    (user-error "No more In nor Out")))

(defvar-keymap pk/ipynb-render-mode-map
  :doc "Keymap for `pk/ipynb-mode'."
  "N"     #'pk/ipynb-next-in-or-out
  "P"     #'pk/ipynb-previous-in-or-out)

(define-derived-mode pk/ipynb-render-mode fundamental-mode "ipynb-render"
  "Major mode to display output of rendered ipynb files."
  (view-mode)
  (setq buffer-read-only t))

(defun pk/ipynb-find-and-render-file (file-name)
  "Find FILE-NAME and open is as html."
  (interactive
   (list (read-file-name "Find ipynb file to render: " nil nil t)))
  (let ((base-name (file-name-nondirectory file-name)))
    (with-temp-buffer
      (insert-file-contents file-name)
      (shell-command-on-region (point-min)
                               (point-max)
                               pk/ipynb--convert-command
                               nil
                               'no-mark)
      (shr-render-buffer (current-buffer)))
    (with-current-buffer "*html*"
      (rename-buffer (format "*%s<render>*" base-name) 'unique)
      (pk/ipynb-render-mode))))

(defun pk/ipynb-render-buffer (buffer)
  "Render ipynb BUFFER as html."
  (interactive
   (list (read-buffer "ipynb buffer to render: " (current-buffer) t)))
  (let ((buffer-name (buffer-name)))
    (with-temp-buffer
      (insert-buffer-substring (get-buffer buffer))
      (shell-command-on-region (point-min)
                               (point-max)
                               pk/ipynb--convert-command
                               nil
                               'no-mark)
      (shr-render-buffer (current-buffer)))
    (with-current-buffer "*html*"
      (rename-buffer (format "*%s<render>*" buffer-name) 'unique)
      (pk/ipynb-render-mode))))

(defun pk/ipynb-find-and-browse-file (file-name)
  "Find FILE-NAME and open is as html."
  (interactive
   (list (read-file-name "Find ipynb file to browse: " nil nil t)))
  (with-temp-buffer
    (insert-file-contents file-name)
    (shell-command-on-region (point-min)
                             (point-max)
                             pk/ipynb--convert-command
                             nil
                             'no-mark)
    (browse-url-of-buffer)))

(defun pk/ipynb-browse-buffer (buffer)
  "Render ipynb BUFFER as html."
  (interactive
   (list (read-buffer "ipynb buffer to browse: " (current-buffer) t)))
  (with-temp-buffer
    (insert-buffer-substring (get-buffer buffer))
    (shell-command-on-region (point-min)
                             (point-max)
                             pk/ipynb--convert-command
                             nil
                             'no-mark)
    (browse-url-of-buffer)))


;; (setf (alist-get 'ruby-mode
;;                             eglot-server-programs
;;                             nil nil
;;                             (lambda (elt key)
;;                               (or (eq key elt)
;;                                   (and (listp elt)
;;                                        (memq key elt)))))
;;       '("solargraph" "socket" "--port" :autoport))

;; (use-package ruby-ts-mode
;;   :after (eglot)
;;   :require nil
;;   :config
;;   (let ((contact (alist-get 'ruby-mode
;;                             eglot-server-programs
;;                             nil nil
;;                             (lambda (elt key)
;;                               (or (eq key elt)
;;                                   (and (listp elt)
;;                                        (memq key elt)))))))
;;     (setf (alist-get 'ruby-mode
;;                      eglot-server-programs
;;                      nil nil
;;                      (lambda (elt key)
;;                        (or (eq key elt)
;;                            (and (listp elt)
;;                                 (memq key elt)))))
;;           (lambda (&optional interactive project)
;;             (let ((project-root (cdr project)))
;;              (if (string-match-p (rx-to-string `(seq string-start ,(or (getenv "HOMEBREW_PREFIX")
;;                                                                       "/opt/homebrew"))
;;                                               'no-group)
;;                                 project-root)
;;                  ;; as in /opt/homebrew/Library/Homebrew/dev-cmd/typecheck.rb
;;                 `("bundle" "exec" "srb" "tc" "--lsp" "--dir" ,project-root)
;;               (if (functionp contact)
;;                   (pcase (cdr (func-arity contact))
;;                     (1 (funcall contact interactive))
;;                     (_ (funcall contact interactive project)))
;;                 contact)))))))


;; packages development
(use-package el-mock
  :defer t)
(use-package undercover
  :defer t)

(use-package casual
  :defer t
  :bind ("C-o" . #'casual-editkit-main-tmenu)
  :init
  (use-package org-agenda
    :ensure nil
    :defer t
    :commands (org-agenda-clock-goto)
    :bind (:map org-agenda-mode-map
           ("C-o" . #'casual-agenda-tmenu)
           ("M-j" . #'org-agenda-clock-goto)
           ("J" . #'bookmark-jump)))
  (use-package bookmark
    :ensure nil
    :defer t
    :bind (:map bookmark-bmenu-mode-map
           ("C-o" .  #'casual-bookmarks-tmenu)
           ("J" . #'bookmark-jump)))
  (use-package calc
    :ensure nil
    :defer t
    :bind (:map calc-mode-map
           ("C-o" . #'casual-calc-tmenu)))
  (use-package calc-ext
    :ensure nil
    :defer t
    :bind (:map calc-alg-map
           ("C-o" . #'casual-calc-tmenu)))
  (use-package calendar
    :ensure nil
    :defer t
    :bind (:map calendar-mode-map
           ("C-o" . #'casual-calendar)))
  (use-package dired
    :ensure nil
    :defer t
    :bind (:map dired-mode-map
           ("C-o" . #'casual-dired-tmenu)
           ("s" . #'casual-dired-sort-by-tmenu)
           ("/" . #'casual-dired-search-replace-tmenu)))
  (use-package ibuffer
    :ensure nil
    :defer t
    :bind (:map ibuffer-mode-map
           ("C-o" . #'casual-ibuffer-tmenu)
           ("F" . #'casual-ibuffer-filter-tmenu)
           ("s".  #'casual-ibuffer-sortby-tmenu)
           ("{" . #'ibuffer-backwards-next-marked)
           ("}" . #'ibuffer-forward-next-marked)
           ("[" . #'ibuffer-backward-filter-group)
           ("]" . #'ibuffer-forward-filter-group)
           ("$" . #'ibuffer-toggle-filter-group)))
  (use-package info
    :ensure nil
    :defer t
    :bind (:map Info-mode-map
           ("C-o" . #'casual-info-tmenu)
           ("M-[" . #'Info-history-back)
           ("M-]" . #'Info-history-forward)
           ("/" . #'Info-search)
           ("B" . #'bookmark-set)))
  (use-package isearch
    :ensure nil
    :defer t
    :bind (:map isearch-mode-map
           ("C-o" . #'casual-isearch-tmenu)))
  (use-package re-builder
    :ensure nil
    :defer t
    :bind (:map reb-mode-map
           ("C-o". casual-re-builder-tmenu)
           :map reb-lisp-mode-map
           ("C-o" . casual-re-builder-tmenu))))


(defvar exordium-vc-checkout-alist)
(unless (featurep 'init-vc-checkout)
  (if (fboundp 'package-vc-install-from-checkout)
      (dolist (spec exordium-vc-checkout-alist)
        (when-let* ((dir (cdr spec))
                    ((file-exists-p dir))
                    (name (symbol-name (car spec)))
                    (pkg-dir (expand-file-name name package-user-dir)))
          (message "Using checked out %s package at %s" name dir)
          (when-let* ((pkg-desc (cadr (assq (intern name) package-alist)))
                      ((not (eq 'vc (package-desc-kind pkg-desc)))))
            (package-delete pkg-desc)
            ;; after uninstall: remove from `load-path'
            (setq load-path (seq-remove
                             (lambda (dir)
                               (string= dir (package-desc-dir pkg-desc)))
                             load-path)))
          (unless (equal (file-truename pkg-dir)
                         (file-truename dir))
            ;; `package-vc-install-from-checkout' complains when the symlink exists
            (when (file-exists-p pkg-dir)
              (delete-file pkg-dir))
            (package-vc-install-from-checkout dir name))))
    (message "Skipping installation of packages from repositories: %s"
             (if (fboundp 'package-vc-install-from-checkout)
                 "no workspace"
               "no `package-vc-install-from-checkout'"))))

(use-package difftastic-bindings
  :ensure difftastic
  :config
  (difftastic-bindings-mode))



(use-package ultra-scroll-mac
  :vc (:url "https://github.com/jdtsmith/ultra-scroll-mac.git" :rev :newest)
  :if (featurep 'mac-win)
  :custom
  (scroll-conservatively 101) ; important for jumbo images
  (scroll-margin 0)
  :config
  (when (fboundp 'ultra-scroll-mac-mode)
    (ultra-scroll-mac-mode)))


;; (use-package jinx
;;   :ensure nil
;;   :diminish t
;;   :hook
;;   (emacs-startup . global-jinx-mode)
;;   :custom
;;   (jinx-languages "en_GB pl_PL")
;;   :bind
;;   (("M-$" . jinx-correct)
;;    ("C-M-$" . jinx-languages))
;;   :init
;;   (remove-hook 'prog-mode-hook #'flyspell-prog-mode)
;;   (remove-hook 'text-mode-hook #'flyspell-mode)
;;   (flyspell-mode-off))


(provide 'after-init)

;;; after-init.el ends here
