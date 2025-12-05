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
  :demand t
  :autoload (modus-themes-get-theme-palette)
  :defines (pk/error-to-unspecified)
  :init
  (use-package custom
    :ensure nil
    :after (modus-themes)
    :custom
    (custom-safe-themes
     (progn
       (append custom-safe-themes
               (cl-remove-if (lambda (theme)
                               (member theme custom-safe-themes))
                             (mapcar #'symbol-name
                                     modus-themes-items))))))

  (use-package helm-rg
    :defer t
    :defines (helm-rg--color-format-argument-alist))

  (use-package auto-dark
    :defer t
    :defines (auto-dark-themes))

  (defun pk/org-src-bloc-face-lang-with-bg-color (color)
    (lambda (lang)
      (list lang `(:background ,color))))

  (defun pk/error-to-unspecified (fun &rest args)
    "Convert errors to `unspecified' when getting face attribute"
    (or (ignore-errors (apply fun args))
        'unspecified))

  ;; Add all your customizations prior to loading the themes
  (defun pk/modus-themes--custom-faces ()
    (require 'modus-themes nil t)
    (require 'auto-dark nil t)
    (dolist (themes auto-dark-themes)
      (dolist (theme themes)
        (load-theme theme t t)))
    ;; Expand macro `modus-themes-with-colors' manually (with an extra `eval'),
    ;; as otherwise compiler generates a warning for each colour from
    ;; `modus-themes' palette.
    ;;
    ;; --- begin `modus-themes-with-colors' macro preface ---
    (let* ((sym (gensym))
           (palette (modus-themes-get-theme-palette nil :with-overrides :with-user-palette))
           (colors (mapcar #'car palette)))
      (eval ;; extra `eval'
       `(let* ((c '((class color) (min-colors 256)))
               (,sym (modus-themes-get-theme-palette nil :with-overrides :with-user-palette))
               ,@(mapcar (lambda (color)
                           (list color
                                 `(modus-themes--retrieve-palette-value ',color ,sym)))
                         colors))
          ;; --- end `modus-themes-with-colors' macro preface ---
          (setq helm-rg--color-format-argument-alist
                `((red :cmd-line "red"
                       :text-property ,fg-completion-match-0)))

          (setopt highlight-symbol-colors `(,bg-yellow-intense
                                            ,bg-magenta-intense
                                            ,bg-cyan-intense
                                            ,bg-green-intense
                                            ,bg-red-intense
                                            ,bg-blue-intense))
          (setopt org-src-block-faces
                  (append
                   (mapcar (pk/org-src-bloc-face-lang-with-bg-color bg-magenta-nuanced)
                           '("clojure" "clojurescript" "elisp" "emacs-lisp" "lisp"
                             "scheme"))
                   (mapcar (pk/org-src-bloc-face-lang-with-bg-color bg-blue-nuanced)
                           '("c" "c++" "fortran" "go" "java" "objc" "rust"))
                   (mapcar (pk/org-src-bloc-face-lang-with-bg-color bg-yellow-nuanced)
                           '("awk" "bash" "ipython" "js" "perl" "python" "r" "ruby"
                             "sed" "sh" "shell" "zsh"))
                   (mapcar (pk/org-src-bloc-face-lang-with-bg-color bg-green-nuanced)
                           '("dot" "html" "latex" "org" "plantuml" "xml"))
                   (mapcar (pk/org-src-bloc-face-lang-with-bg-color bg-red-nuanced)
                           '("css" "scss" "sql"))
                   (mapcar (pk/org-src-bloc-face-lang-with-bg-color bg-cyan-nuanced)
                           '("conf" "docker" "json" "makefile" "yaml"))))

          (let ((helm-faces
                 (list ;; Redoing helm, inspired by last removed version in:
                  ;; https://github.com/protesilaos/modus-themes/commit/1efaa7ef79682ec13493351d52ed1b339fb6ace2
                  `(helm-selection ((t (,@c :inherit modus-themes-completion-selected))))
                  `(helm-match ((t (,@c :inherit modus-themes-completion-match-0))))
                  `(helm-match-item ((t (,@c :inherit modus-themes-completion-match-0))))
                  `(helm-visible-mark ((t (,@c :background ,bg-cyan-nuanced))))
                  `(helm-source-header ((t (,@c :foreground ,cyan :inherit modus-themes-heading-1))))
                  `(helm-header-line-left-margin ((t (,@c :inherit bold :foreground ,yellow-intense))))
                  `(helm-candidate-number ((t (,@c :foreground ,cyan))))
                  `(helm-candidate-number-suspended ((t (,@c :foreground ,yellow))))
                  `(helm-locate-finish ((t (,@c :inherit success))))
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
                  `(helm-M-x-short-doc ((t (,@c :inherit completions-annotations))))
                  `(helm-completions-annotations ((t (,@c :inherit completions-annotations))))
                  `(helm-completions-detailed ((t (,@c :inherit completions-annotations))))
                  `(helm-completions-invalid ((t (,@c :inherit font-lock-warning-face))))))

                (exordium-org-faces
                 (mapcar
                  (lambda (elt)
                    `(,(car elt) ((t (,@c :inherit 'org-todo :foreground ,(cdr elt))))))
                  `((exordium-org-work . ,magenta-cooler)
                    (exordium-org-wait . ,cyan)
                    (exordium-org-stop . ,fg-dim))))

                (iedit-faces
                 (mapcar
                  (lambda (elt)
                    `(,(car elt)
                      ((t (,@c
                           :background unspecified :foreground unspecified
                           :box (:line-width -2 ,@(when-let* ((color (cdr elt))
                                                              ((not (eq color 'unspecified))))
                                                    (list :color color))))))))
                  `((iedit-occurrence . ,fg-completion-match-0)
                    (iedit-read-only-occurrence . ,fg-completion-match-1))))

                (tab-bar-faces
                 (mapcar
                  (lambda (elt)
                    ;; mashup of modus-themes with https://github.com/aaronjensen/emacs-modern-tab-bar
                    `(,(car elt) ((t (,@c :box (:line-width (20 . 4) :color ,(cdr elt) :style flat-button)
                                          :background ,(cdr elt))))))
                  `((tab-bar-tab . ,bg-tab-current)
                    (tab-bar-tab-inactive . ,bg-tab-bar))))

                ;; FIXME: `dots' seems to be unavailable
                ;; (writegood-faces
                ;;  (mapcar
                ;;   (lambda (face)
                ;;     `(,face ((t (,@c :underline
                ;;                      ,(if-let* ((underline (face-attribute face :underline))
                ;;                                 ((plistp underline)))
                ;;                         (plist-put underline :style 'dots)
                ;;                         underline))))))
                ;;   `(writegood-weasels-face
                ;;     writegood-duplicates-face
                ;;     writegood-passive-voice-face)))

                (other-faces
                 (list
                  ;; Something mid Feb 2025 something is messing up with
                  ;; default face, setting its foreground to "unspecifed-fg"
                  ;; and background to "unspecified-bg". Until that's
                  ;; pinpointed and eliminated, forcibly fix it on user level.
                  `(default ((t (,@c :background ,bg-main :foreground ,fg-main))))

                  `(fixed-pitch ((t (,@c :family ,(face-attribute 'default :family)
                                         :height ,(face-attribute 'default :height)))))
                  `(scroll-bar ((t (,@c :background ,bg-inactive :foreground ,fg-dim))))
                  `(fill-column-indicator ((t (,@c :height 1.0
                                                   :background ,bg-main :foreground ,bg-inactive))))

                  `(highlight-symbol-face ((t (,@c :background ,bg-cyan-nuanced))))
                  `(aw-leading-char-face ((t (,@c :foreground ,red-intense :bold t :height 1.5))))
                  ;; From `objc-font-lock'
                  `(objc-font-lock-background ((t (,@c :slant italic)))))))

            (apply #'custom-theme-set-faces
                   (cons 'user
                         (append other-faces
                                 ;; FIXME: `dots' seems to be unavailable
                                 ;; writegood-faces
                                 tab-bar-faces
                                 iedit-faces
                                 exordium-org-faces
                                 helm-faces)))))))

    (when (fboundp #'posframe-delete-all)
      (posframe-delete-all)))

  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-completions '((matches . (extrabold background intense))
                              (selection . (semibold accented intense))
                              (popup . (semibold accented))))
  (modus-themes-headings `((agenda-date . (,exordium-height-plus-1))
                           (agenda-structure . (variable-pitch ,exordium-height-plus-2))
                           (0 . (,exordium-height-plus-4))
                           (1 . (variable-pitch rainbow regular ,exordium-height-plus-4))
                           (2 . (variable-pitch rainbow regular ,exordium-height-plus-3))
                           (3 . (variable-pitch rainbow regular ,exordium-height-plus-2))
                           (4 . (variable-pitch rainbow regular ,exordium-height-plus-1))
                           (t . (variable-pitch rainbow regular))))

  :config
  ;; Workaround for fonts not being available on all frames
  (advice-add #'internal-get-lisp-face-attribute
              :around #'pk/error-to-unspecified)

  (setopt modus-themes-common-palette-overrides
          `(;; (border-mode-line-active unspecified)
            ;; (border-mode-line-inactive unspecified)
            (bg-mode-line-active bg-blue-subtle)
            (fg-mode-line-active fg-main)
            (fg-region unspecified)
            (overline-heading-1 fg-main)
            ,@modus-themes-preset-overrides-faint))
  :hook
  (modus-themes-after-load-theme . pk/modus-themes--custom-faces)
  :bind
  ("<f5>" . modus-themes-toggle))

(use-package auto-dark
  :demand t
  :diminish
  :custom
  (auto-dark-themes '((modus-vivendi) (modus-operandi)))
  (auto-dark-detection-method (when (getenv "ci_tests")
                                'none))
  :hook
  (auto-dark-dark-mode . pk/modus-themes--custom-faces)
  (auto-dark-light-mode . pk/modus-themes--custom-faces)
  :config
  (auto-dark-mode)
  (pk/modus-themes--custom-faces))



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
(setq uniquify-dirname-transform 'project-uniquify-dirname-transform)

;; The following needs an smtp.gmail.com entry in `auth-sources'
(use-package smtpmail
  :ensure nil
  :custom
  ;; (smtpmail-debug-info t)
  ;; (smtpmail-debug-verb t)
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  (smtpmail-servers-requiring-authorization (rx "gmail.com" string-end))
  (smtpmail-smtp-user "pkryger@gmail.com")
  (smtpmail-mail-address "prkyger@gmail.com"))

(use-package message
  :ensure nil
  :custom
  (message-send-mail-function #'smtpmail-send-it))

(use-package sendmail
  :ensure nil
  :custom
  (send-mail-function #'smtpmail-send-it))



(use-package window
  :ensure nil
  :custom
  ;; Typical full screen window on built in macBook Air M2 13" is 361
  ;; columns. This includes line numbers and fringes.  Yet windows as small as
  ;; 160 seems to be quite all right fitting horizontal split.  On the other
  ;; hand, typical full screen window height on external 4k screen connected to
  ;; the same laptop is 88 and 55 on the built in panel.  See
  ;; `split-window-sensibly' doc.  With setting
  ;; `split-window-preferred-direction' to `longest' this setup basically
  ;; forces a horizontal split, but only up to 2 windows in a frame, followed
  ;; by an vertical split.  Values used are compared against:
  ;;
  ;; (window-width (selected-window))
  ;; (window-height (selected-window))
  ;;
  ;; For testing use:
  ;;
  ;; (split-window-sensibly)
  (split-height-threshold 56)
  (split-width-threshold 179))

(use-package helm-core
  :ensure nil
  :custom
  (helm-split-width-threshold split-width-threshold))


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

(use-package abbrev
  :ensure nil
  :init
  (use-package markdown-mode
    :ensure nil
    :defer t
    :defines (markdown-mode-abbrev-table))
  :config
  (define-abbrev global-abbrev-table "shrug" pk/shrug-string)
  (define-abbrev markdown-mode-abbrev-table "shrug"
    (replace-regexp-in-string (rx (group (or "\\" "_")))
                              (rx "\\" (backref 1))
                              pk/shrug-string)))


(use-package helm
  :diminish
  :functions (pk/helm-locate-library-scan-alist
              pk/async-locate-library-scan
              pk/async-info-files-scan)
  :init
  (use-package find-func
    :ensure nil
    :defer t
    :autoload (find-library-suffixes))
  (use-package async
    :defer t
    :autoload (async-inject-variables))
  (use-package helm-lib
    :ensure helm
    :defer t
    :autoload (helm-basename))
  (use-package helm-elisp
    :ensure helm
    :defer t
    :defines (helm--locate-library-cache))
  (use-package helm-info
    :ensure helm
    :defer t)

  (defun pk/async-info-files-scan ()
  (async-start
   `(lambda ()
      (require 'info)
      ,(async-inject-variables (rx string-start
                                   (or "load-path"
                                       "Info-directory-list"
                                       "Info-additional-directory-list"
                                       "Info-suffix-list")
                                   string-end))
      (require 'async)
      (require 'helm-info)
      (require 'helm-lib)
      (let* ((Info-directory-list (if (version< emacs-version "30")
                                      (append Info-directory-list ; Until Emacs-29
                                              Info-additional-directory-list)
                                    Info-directory-list))
             (info-index-list (helm-get-info-files))
             ;; Use what `info-display-manual' and `helm-info' would
             (infos
              (helm-fast-remove-dups
               (append info-index-list
                       (info--filter-manual-names (info--manual-names nil)))
               :test #'equal))
             (info-files
              (delq nil (mapcar (lambda (info)
                                   (when-let* ((file (Info-find-file info t)))
                                     (cons info file)))
                                 infos)))
             (info-files-docs
              (delq nil (mapcar (lambda (info-file)
                                  (when-let* ((file (cdr info-file))
                                              (doc (helm-info-file-doc file)))
                                    (cons file doc)))
                                info-files))))
        (async-send
         :info-index-list info-index-list
         :info-files info-files
         :info-files-docs info-files-docs)))

   (lambda (result)
     (when (plistp result)
       (when-let* ((info-index-list (plist-get result :info-index-list)))
         (setq helm-default-info-index-list info-index-list))
       (when-let* ((info-files (plist-get result :info-files)))
         (if helm-info--files-cache
             (dolist (file info-files)
               (setf (alist-get (car file) helm-info--files-cache)
                     (cdr file)))
           (setq helm-info--files-cache info-files)))
       (dolist (manual-summary (plist-get result :info-files-docs))
         (puthash (car manual-summary) (cdr manual-summary)
                  helm-info--files-doc-cache))))))

  (defun pk/helm-locate-library-scan-alist ()
    "Return alist of libraries in `load-path' or in `find-library-source-path'.
Each element is in a form of (BASENAME . PATH) where BASENAME is
the library and PATH is the file containing the library."
    (require 'find-func)
    (let ((cache (make-hash-table :test 'equal))
          (dirs (or find-library-source-path load-path))
          (suffixes-pat (concat (regexp-opt (find-library-suffixes))
                                (rx string-end)))
          (skip-files-pat
           (rx (or
                ;; A lock file in LOAD-PATH (bug#2626).
                (seq string-start ".#")
                ;; .dir-locals that are in VC installed packages and file
                ;; load-path/find-library-source-path entries
                (seq string-start ".dir-locals"
                     (zero-or-one "-2") string-end)
                ;; Sometimes temporary files created by flycheck are picked up.
                (seq string-start "flycheck_")))))
      (delq
       nil
       (apply
        #'append
        (mapcar (lambda (dir)
                  (when (and (file-exists-p dir)
                             (file-directory-p dir))
                    (mapcar (lambda (file)
                              (when-let* ((basename (helm-basename file 2))
                                          ;; Only the first occurrence
                                          ((not (gethash basename cache)))
                                          ((not (string-match-p skip-files-pat
                                                                basename)))
                                          ;; Skip directories, pipes, etc.
                                          (path (file-name-concat dir file))
                                          ((file-regular-p path)))
                                (puthash basename t cache)
                                (cons basename path)))
                            (directory-files dir nil suffixes-pat t))))
                dirs)))))

  (defun pk/async-locate-library-scan ()
    "Scan libraries and their documentation."
    (require 'helm-elisp)
    ;; this scan should be reasonably fast, below 400ms (usually ~200ms) on
    ;; MacBook Air M2 with ~2200 libraries
    (let ((libraries (pk/helm-locate-library-scan-alist)))
      (if helm--locate-library-cache
          ;; Update cached mappings to possibly new locations
          (dolist (library libraries)
            (setf (alist-get (car library)
                             helm--locate-library-cache
                             nil nil #'equal)
                  (cdr library)))
        (setq helm--locate-library-cache libraries))
      ;; Scanning documentation is quite slow, 4.5s to 5s on MacBook Air M2
      ;; with ~2200 libraries.  To avoid UI blocking, do it in an asynchronous
      ;; process updating cache in a sentinel.
      (async-start
       `(lambda ()
          ,(async-inject-variables (rx string-start
                                       (or "load-path"
                                           "helm--locate-library-cache")
                                       string-end))
          (require 'helm-lib)
          (require 'async)
          ;; Using `async-send' as "just" returning a result sometimes fails
          ;; ¯\_(ツ)_/¯
          (async-send :docs-alist
                      (delq
                       nil
                       (mapcar
                        (lambda (entry)
                          (pcase-let* ((`(,basename . ,path) entry))
                            (when (and basename path
                                       (file-readable-p path))
                              (cons basename (helm-locate-lib-get-summary path)))))
                        helm--locate-library-cache))))
       (lambda (result)
         (when-let* (((plistp result))
                     (docs-alist (plist-get result :docs-alist)))
           (dolist (entry docs-alist)
             (pcase-let* ((`(,basename . ,doc) entry))
               ;; Update cache in case doc has changed (i.e., in a VC
               ;; installed package)
               (puthash basename doc helm--locate-library-doc-cache))))))))

  (defun pk/async-helm-cache-scan-when-idle (&rest _)
    "Run async library scan next time Emacs is idle for 5 seconds"
    (run-with-idle-timer 5 nil #'pk/async-locate-library-scan)
    (run-with-idle-timer 5 nil #'pk/async-info-files-scan))

  :hook
  (emacs-startup . pk/async-helm-cache-scan-when-idle)
  :config
  (advice-add 'package-install :after
              #'pk/async-helm-cache-scan-when-idle)
  (advice-add 'package-vc-install :after
              #'pk/async-helm-cache-scan-when-idle))


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
  (use-package vc-git
    :ensure nil
    :autoload (vc-git-checkout))
  (use-package ansii-color
    :ensure nil
    :autoload (ansi-color-apply))
  
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
  (put 'flycheck-emacs-lisp-load-path 'risky-local-variable nil)
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

  ;; Need to periodically run:
  ;;   cd /opt/homebrew/Library/Homebrew && bundle install --gemfile /opt/homebrew/Library/Homebrew/Gemfile
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
                                   (project-root (project-current))))
                  (library (expand-file-name "Library" homebrew-prefix))
                  (display-buffer-alist '((t . (display-buffer-no-window))))
                  (default-directory library))
        ;; Sometimes Gemfile.lock gets changed, let's reset it to HEAD.
        (vc-git-checkout "Homebrew/Gemfile.lock")
        ;; Ensure sorbet and gems are up to date, either by running "bundle
        ;; install" or by falling back to "brew typecheck"
        (if-let* ((direnv (executable-find "direnv"))
                  ((file-exists-p (expand-file-name ".envrc" library))))
            (async-shell-command
             (format (concat "%s exec %s bundle update --bundler"
                             " && %s exec %s bundle install --gemfile %s")
                     direnv library
                     direnv library (expand-file-name "Homebrew/Gemfile"
                                                      library)))
          (warn "[pk/sorbet-homebrew] Missing %s"
                (if direnv
                    (concat (expand-file-name ".envrc" library)
                            " - install from https://github.com/pkryger/dotfiles")
                  "direnv - install with \"brew install direnv\""))
          (async-shell-command "brew typecheck --update"))
        t)))

  (setopt pk/sorbet-homebrew
          (if-let* ((homebrew-prefix (getenv "HOMEBREW_PREFIX"))
                    (library (expand-file-name "Library" homebrew-prefix))
                    (direnv (executable-find "direnv"))
                    ((file-exists-p (file-name-concat library ".envrc")))
                    (stderr (make-temp-file "pk-sorbet-homebrew-"))
                    (bundle (string-trim
                             (with-output-to-string
                               (call-process direnv
                                             nil (list standard-output stderr) nil
                                             "exec" library "which" "bundle"))))
                    ((not (string-empty-p bundle))))
              (progn
                (delete-file stderr)
                bundle)
            (warn "[pk/sorbet-homebrew] Missing %s"
                  (cond
                   (stderr
                    (prog1
                        (format "bundle executable: %s"
                                (with-temp-buffer
                                  (insert-file-contents stderr)
                                  (ansi-color-apply (buffer-string))))
                      (delete-file stderr)))
                   (direnv
                    (concat (expand-file-name ".envrc" library)
                            " - install from https://github.com/pkryger/dotfiles"))
                   (t
                    "direnv - install with \"brew install direnv\"")))
            "bundle"))

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


(use-package objc-font-lock
  :config
  (objc-font-lock-global-mode 1))


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
  :diminish
  :custom
  (eldoc-idle-delay 0.25)
  (eldoc-echo-area-prefer-doc-buffer t))


(use-package paredit
  :commands (paredit-RET)
  :functions (pk/paredit-RET
              pk/suppress-paredit-mode-for-active-region-or-prefix-arg)
  :init
  (use-package ielm
    :ensure nil
    :commands (ielm-return))
  (use-package rect
    :ensure nil
    :commands (rectangle-mark-mode))

  (defun pk/paredit-RET ()
    "Call `paredit-RET' unless special handling is required."
    (interactive)
    (cond ((derived-mode-p 'inferior-emacs-lisp-mode)
           (ielm-return))
          ((minibufferp)
           (read--expression-try-read))
          (t (paredit-RET))))

  (defun pk/suppress-paredit-mode-for-rectangle-mark-mode ()
    "Suppress `paredit-mode' when entering `rectangle-mark-mode'."
    (if rectangle-mark-mode
        (paredit-mode -1)
      (paredit-mode)))

  (defun pk/suppress-paredit-mode-for-active-region-or-prefix-arg (orig-fun &rest args)
    "Suppress `paredit-mode' when region is active or car ARGS is non number."
    (let* ((arg (car-safe args))
           (suppress-paredit (and paredit-mode
                                  (or (region-active-p)
                                      (and arg (not (numberp arg)))))))
      (when suppress-paredit
        (paredit-mode -1))
      (unwind-protect
          (apply orig-fun args)
        (when suppress-paredit
          (paredit-mode)))))

  :bind
  (:map paredit-mode-map
        ("RET" . #'pk/paredit-RET))
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (ielm-mode . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode)
         (rectangle-mark-mode . pk/suppress-paredit-mode-for-rectangle-mark-mode))
  :config
  (advice-add #'undo
              :around #'pk/suppress-paredit-mode-for-active-region-or-prefix-arg))


(use-package dumb-jump
  :defer t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


;; Spell check suggestion following actual implementation:
;; https://github.com/redguardtoo/emacs.d/blob/990f4af/lisp/init-spelling.el
;; And a few blog articles
;; http://redguardtoo.github.io/posts/effective-spell-check-in-emacs.html
;; http://redguardtoo.github.io/posts/how-to-spell-check-functionvariable-in-emacs.html
(use-package ispell
  ;;   :ensure-system-package aspell
  :defer t
  :functions (pk/ispell-hack-extra-args)
  :init
  (defun pk/ispell-hack-extra-args (orig-fun &rest args)
    "Remove camel case when fixing a typo.
This is to reduce the number of proposals."
    (let ((ispell-extra-args '("--sug-mode=ultra")))
      (ispell-kill-ispell t)
      (prog1
          (apply orig-fun args)
        (ispell-kill-ispell t))))
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args `("--sug-mode=ultra"
                       ,@(if (string-match (rx "--" (zero-or-one "[dont-]") "camel-case")
                                           (shell-command-to-string "aspell --help"))
                             '("--camel-case")
                           '("--run-together"
                             "--run-together-limit=16"))))
  (ispell-dictionary "british")
  :config
  (advice-add #'ispell-word :around #'pk/ispell-hack-extra-args))

(use-package flyspell
  :diminish
  :defer t
  :autoload (flyspell-auto-correct-word)
  :custom
  (flyspell-issue-message-flag nil)
  :hook
  ((git-commit-mode . flyspell-mode)
   (org-mode        . flyspell-mode)
   (text-mode       . flyspell-mode))
  :bind
  (([remap ispell-word] . #'flyspell-correct-wrapper))
  :config
  (advice-add #'flyspell-auto-correct-word :around #'pk/ispell-hack-extra-args))

(use-package flyspell-correct
  :defer t
  :config
  (advice-add #'flyspell-correct-at-point :around #'pk/ispell-hack-extra-args))

(use-package flyspell-correct-helm
  :after (flyspell flyspell-correct)
  :defer t
  :autoload (flyspell-correct-helm)
  :custom
  (flyspell-correct-interface #'flyspell-correct-helm))



(set-time-zone-rule "/usr/share/zoneinfo/Europe/London")


(use-package groovy-mode
  :defer t
  :init
  (defun pk/groovy-mode--create-test-files ()
    (setq-local projectile-create-missing-test-files t))
  ;; Jenkinsfile is registered in autoloads of groovy-mode, let's remove it
  ;; and make it clear that jenkinsfile-mode will take over
  (setq auto-mode-alist
        (cl-remove-if (lambda (entry)
                        (and (string-match-p "Jenkinsfile" (car entry))
                             (eq 'groovy-mode (cdr entry))))
                      auto-mode-alist))  :hook
  ((groovy-mode . yas-minor-mode)
   (groovy-mode . pk/groovy-mode--create-test-files)))

(use-package jenkinsfile-mode
  :after (groovy-mode)
  :init
  (use-package flycheck
    :autoload (flycheck-add-mode))
  :config
  (flycheck-add-mode 'groovy 'jenkinsfile-mode))

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
  :custom
  (projectile-auto-cleanup-known-projects t)
  (projectile-auto-discover t)
  (projectile-project-search-path (list (cons "~/gh" 2)
                                        package-user-dir
                                        user-emacs-directory
                                        (expand-file-name "taps"
                                                          user-emacs-directory))))

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
  :init
  (use-package yaml-pro
    :defer t
    :autoload (yaml-pro-ts-next-subtree
               yaml-pro-ts-prev-subtree
               yaml-pro-move-subtree-down
               yaml-pro-move-subtree-up)
    :bind
    (:map yaml-pro-ts-mode-map
          ("M-n" . #'yaml-pro-ts-next-subtree)
          ("M-p" . #'yaml-pro-ts-prev-subtree)
          ("M-N" . #'yaml-pro-move-subtree-down)
          ("M-P" . #'yaml-pro-move-subtree-up)))
  :defer t
  :mode (rx ".y" (zero-or-one "a") "ml.template" string-end)
  :hook
  ;; (yaml-mode . yaml-pro-mode)
  (yaml-ts-mode . yaml-pro-ts-mode))


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



(use-package deft
  :config
  (setq deft-extensions '("org" "txt" "md"))
  (setq deft-default-extension "org")
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 0))


(defun pk/read-only-mode-maybe ()
  "Turn on `read-only-mode' if the current buffer visits Emacs or ELPA file."
  (when-let* ((file (buffer-file-name))
              ((string-match-p
                (rx-to-string
                 `(seq string-start
                       (or ,(expand-file-name
                             (file-name-parent-directory data-directory))
                           ,(expand-file-name
                             (file-name-as-directory package-user-dir)))))
                file)))
    (read-only-mode)))
(add-hook 'find-file-hook #'pk/read-only-mode-maybe)


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
    :defer t
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
    :defer t
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

  :functions (pk/swiper-C-r
              pk/swiper-iedit)
  :bind
  (("C-s" . #'swiper-isearch)
   ("C-r" . #'swiper-isearch-backward)
   :map swiper-map
   ("C-r" . #'pk/swiper-C-r)
   ("C-c ;" . #'pk/swiper-iedit)))



(use-package ob-python
  :ensure nil
  :custom
  (org-babel-python-command "python3"))

(use-package org
  :ensure nil
  :custom
  (org-src-tab-acts-natively t))


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
  :after restclient)
(use-package ob-restclient
  :after restclient
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
           '((restclient . t)))))

(use-package company-restclient
  :after restclient
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
          ;; Allow for resolution/matrix like strings, e.g., 12x34.
          (set-char-table-range composition-function-table
                                  '(?0 . ?9)
                                  '([".\\(?:x[[:digit:]]\\)" 0
                                     mac-auto-operator-composition-shape-gstring]))
          ;; Hex numbers, e.g., 0xa, #xa #x1
          ;; This comes later to overwrite the resolution/matrix for hex digits
          (dolist (char '(?0 ?#))
            (set-char-table-range composition-function-table
                                  char
                                  '([".\\(?:x[[:xdigit:]]\\)" 0
                                     mac-auto-operator-composition-shape-gstring])))

          (global-auto-composition-mode 1))
      (map-char-table
       (lambda (c rules)
         (when (consp rules)
           (let (new-rules removed-p)
             (dolist (rule rules)
               (if (eq (aref rule 2) 'mac-auto-operator-composition-shape-gstring)
                   (setq removed-p t)
                 (push rule new-rules)))
             (when removed-p
               (set-char-table-range composition-function-table c
                                       (nreverse new-rules))))))
       composition-function-table)
      (clrhash mac-auto-operator-composition-cache))))

(defun pk/org-prettify-bullet-lists ()
  "Prettify `org-mode' list bullets."
  (let ((bullets '(("[ ]" . ?\u2610) ; ballot box
                   ("[X]" . ?\u2611) ; ballot box with check
                   ("[-]" . ?\u25a3)))) ; white square containing black small square
    (when (cl-every (lambda (symbol)
                      (char-displayable-p (cdr symbol)))
                    bullets)
      (setq prettify-symbols-alist (append bullets prettify-symbols-alist))
      (prettify-symbols-mode))))

(add-hook 'org-mode-hook #'pk/org-prettify-bullet-lists)

;; Helm and ediff are having issues with ligatures
(add-hook 'helm-major-mode-hook
          (lambda ()
            (auto-composition-mode -1)))
;; Disabling ligatures in ediff mode only removes them in the ediff buffer
;; itself (the small buffer underneath) and not the buffers you compare. Which
;; is probably a preferred solution.
(add-hook 'ediff-mode-hook
          (lambda ()
            (auto-composition-mode -1)))

(when (fboundp 'pk/mac-auto-operator-composition-mode)
  (pk/mac-auto-operator-composition-mode))

(use-package ediff
  :ensure nil
  :defer t
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain))


(use-package view-mode
  :ensure nil
  :defer t
  :hook diff-mode)


(add-hook 'git-commit-mode-hook 'turn-on-auto-fill)
(use-package forge
  :config
  (add-to-list 'forge-owned-accounts '("pkryger" . (remote-name "pkryger")))
  (add-to-list 'forge-owned-accounts '("emacs-exordium" . (remote-name "exordium"))))


(when-let*  ((font-and-size (car (cl-remove-if-not
                                  (lambda (font-and-size)
                                    (member (car font-and-size)
                                            (font-family-list)))
                                  (bound-and-true-p exordium-preferred-variable-fonts)))))
  (set-face-attribute 'variable-pitch nil
                      :family (car font-and-size)
                      :height (cdr font-and-size)
                      :weight 'normal))


;; Configure tabs, based on https://github.com/aaronjensen/emacs-modern-tab-bar
(use-package tab-bar
  :ensure nil
  :functions (pk/tab-bar--format-tab)
  :init
  (defun pk/tab-bar--format-tab (orig-fn tab i)
                                        ; checkdoc-params: (orig-fn i)
    "Select appropriate `tab-bar-separator' for TAB."
    (let* ((tabs (funcall tab-bar-tabs-function))
           (previous-tab (when (> i 1)
                           (nth (- i 2) tabs)))
           (tab-bar-separator (cond ((eq 1 i)
                                     (propertize " "
                                                 'face 'tab-bar-tab-inactive
                                                 'display '((space :width 4))))
                                    ((or (eq (car tab) 'current-tab)
                                         (eq (car previous-tab) 'current-tab))
                                     (propertize ""
                                                 'face 'tab-bar-tab-inactive))
                                    ((stringp tab-bar-separator)
                                     (propertize tab-bar-separator
                                                 'face 'tab-bar-tab-inactive
                                                 'display '((height .8)))))))
      (funcall orig-fn tab i)))

  (defun pk/tab-bar--tab-name-format (tab i)
                                        ; checkdoc-params: (i)
    "Add extra horizontal padding and ⌘ hint (on macOS) for TAB."
    (let ((face (funcall tab-bar-tab-face-function tab))
          (mac-hint (and tab-bar-tab-hints exordium-osx)))
      (concat
       (propertize " "
                   'face face
                   'display '((space :width 8)))
       (let ((tab-bar-tab-hints (unless mac-hint
                                  tab-bar-tab-hints)))
         (tab-bar-tab-name-format-default tab i))
       (propertize " "
                   'face face
                   'display `((space :width
                                     ,(- 8 (if (and mac-hint (< i 9)) 2 0)))))
       (when (and mac-hint (< i 9))
         (concat
          (propertize (format "⌘%d " i)
                      'face face)
          (propertize " "
                      'face face
                      'display '((space :width 2))))))))

  :custom
  (tab-bar-separator "|") ; alternative: "¦" or "⦙" or "I" (for SF Pro)
  (tab-bar-tab-name-format-function #'pk/tab-bar--tab-name-format)
  (tab-bar-format '(tab-bar-format-tabs
                    (lambda () (make-string 1 ? ))))
  (tab-bar-auto-width nil)
  (tab-bar-tab-hints t)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-new-tab-choice 'clone)
  (tab-bar-select-tab-modifiers '(hyper))
  (tab-bar-show t)
  :bind
  (("M-<tab>" . #'tab-next)
   ("M-S-<tab>" . #'tab-previous)
   ("H-t" . #'tab-new)
   ("H-w" . #'tab-close))
  :config
  (advice-add 'tab-bar--format-tab :around
              #'pk/tab-bar--format-tab)
  (tab-bar-mode))


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
  (transient-append-suffix 'magit-file-dispatch "g" '(1 "f" "Copy link" git-link)))


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

(use-package org-make-toc)

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
  :defer t
  :custom
  (dired-chown-program (or (executable-find "gchown") "chown"))
  (dired-touch-program (or (executable-find "gtouch") "touch"))
  (dired-use-ls-dired 'unspecified)
  (dired-dwim-target t)
  :config
  (when (version< emacs-version "30")
    (setq insert-directory-program (or (executable-find "gls") "ls"))))

(use-package dired-du
  :defer t
  :custom
  (dired-du-used-space-program `(,(or (executable-find "gdu") "du") "-sb")))


;; Workaround for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=77944
;; Until Emacs-30.2
(use-package man
  :ensure nil
  :defer t
  :autoload (Man-init-defvars)
  :custom
  (Man-sed-command (or (executable-find "gsed") "sed"))
  (Man-awk-command (or (executable-find "gawk") "awk"))
  :config
  (Man-init-defvars))

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


(define-obsolete-function-alias
  'pk/rename-file-and-buffer #'rename-visited-file "29.1")
(define-obsolete-function-alias
 'pk/move-buffer-file #'rename-visited-file "29.1")


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


(use-package cperl-mode
  :ensure nil
  :init
  (add-to-list 'major-mode-remap-alist
               '(perl-mode . cperl-mode))
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

(use-package ipynb
  :defer t
  :exordium-vc-checkout "~/gh/pkryger/ipynb.el"
  :vc (:url "https://github.com/pkryger/ipynb.el.git"
       :rev :newest))

(use-package basic-stats
  :defer t
  :exordium-vc-checkout "~/gh/pkryger/basic-stats.el"
  :vc (:url "https://github.com/pkryger/basic-stats.el.git"
       :rev :newest))

;; Experimental setup for Homebrew ruby
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
(use-package org-commentary
  :defer t
  :exordium-vc-checkout "/Users/pkryger/gh/pkryger/org-commentary.el"
  :vc (:url "https://github.com/pkryger/org-commentary.el.git"
       :rev :newest))

(use-package checkdoc
  :ensure nil
  :functions (pk/checkdoc--supress-cask-emacs)
  :autoload (checkdoc-in-example-string-p)
  :init
  (defun pk/checkdoc--supress-cask-emacs (&rest _)
    (save-match-data
      (looking-back (rx symbol-start "cask emacs") (pos-bol))))
  :config
  (advice-add #'checkdoc-in-example-string-p
              :after-until #'pk/checkdoc--supress-cask-emacs))

(use-package package-build
  :config
  (when-let* ((dir "~/gh/pkryger/melpa-local-recipes")
              ((file-directory-p dir)))
    (setopt package-build-working-dir (expand-file-name "working/" dir))
    (setopt package-build-archive-dir (expand-file-name "packages/" dir))
    (setopt package-build-recipes-dir (expand-file-name "recipes/" dir))))

(use-package writegood-mode
  :defer t)

(use-package powershell
  :mode ((rx ".ps" (zero-or-one (or "m" "d")) "1" string-end)
         . powershell-mode))



(use-package ultra-scroll
  :demand t
  :vc (:url "https://github.com/jdtsmith/ultra-scroll.git" :rev :newest)
  :functions (pk/vscroll-advice-spec
              pk/maybe-disable-vscroll
              pk/maybe-disable-vscroll-previous
              pk/maybe-disable-vscroll-next
              pk/maybe-reset-vscroll
              pk/vscroll-advices
              pk/ultra-scroll-advices)
  :init
  (defun pk/vscroll-advice-spec (spec)
    "Eval interactive SPEC and preppend it with `pk/interactive'."
    (let ((args (advice-eval-interactive-spec spec)))
      (cons 'pk/interactive args)))

  (defun pk/maybe-disable-vscroll (op orig-fun args)
    "Maybe disable vscroll when caling ORIG-FUN.
The vscroll is disabled unless moving line according to OP
by (car ARGS) or (caar ARGS) would move point to an (partially)
invisible line."
    (let* ((interactive (eq (car args) 'pk/interactive))
           (args (if interactive (cdr args) args)))
      (if (and interactive
               (save-excursion
                 (forward-line (funcall op (or (car args) 1)))
                 (pos-visible-in-window-p (point))))
          (cl-letf (((symbol-function #'set-window-vscroll) #'ignore))
            (apply orig-fun args))
        (apply orig-fun args))))

  (defun pk/maybe-disable-vscroll-previous (orig-fun &rest args)
    "Maybe disable vscroll when calling ORIG-FUN.
The vscroll is disabled unless moving line backward by (car ARGS)
would move point to an (partially) invisible line."
    (interactive #'pk/vscroll-advice-spec)
    (pk/maybe-disable-vscroll #'- orig-fun args))

  (defun pk/maybe-disable-vscroll-next (orig-fun &rest args)
    "Maybe disable vscroll when calling ORIG-FUN.
The vscroll is disabled unless moving line forward by (car ARGS)
would move point to an (partially) invisible line."
    (interactive #'pk/vscroll-advice-spec)
    (pk/maybe-disable-vscroll #'+ orig-fun args))

  (defun pk/maybe-disable-vscroll-same (orig-fun &rest args)
    "Maybe disable vscroll when calling ORIG-FUN.
The vscroll is disabled unless moving line forward by (1- (car ARGS))
would move point to an (partially) invisible line."
    (interactive #'pk/vscroll-advice-spec)
    (pk/maybe-disable-vscroll #'1- orig-fun args))

  (defun pk/maybe-reset-vscroll ()
    "Reset vscroll when point is in a (partially) invisible line."
    (unless (or
             (memq this-command '(self-insert-command
                                  ultra-scroll-mac
                                  ultra-scroll))
             (minibufferp)
             (pos-visible-in-window-p (point)))
      (set-window-vscroll nil 0 t)))

  (defun pk/vscroll-advices (action)
    "Take ACTION on advices for vscroll for point movement in `ultra-scroll' mode."
    (let ((args (when (eq action 'advice-add) '(:around))))
      (dolist (fun '(previous-line
                     previous-logical-line
                     dired-previous-line
                     magit-prefious-line))
        (apply action `(,fun ,@args pk/maybe-disable-vscroll-previous)))
      (dolist (fun '(next-line
                     next-logical-line
                     dired-next-line
                     magit-next-line))
        (apply action `(,fun ,@args pk/maybe-disable-vscroll-next)))
      (dolist (fun '(move-beginning-of-line
                     move-end-of-line
                     beginning-of-visual-line
                     end-of-visual-line))
        (apply action `(,fun ,@args pk/maybe-disable-vscroll-same)))
      (if (eq action 'advice-add)
          (add-hook 'post-command-hook #'pk/maybe-reset-vscroll)
        (remove-hook 'post-command-hook #'pk/maybe-reset-vscroll))))

  (defun pk/remove-vscroll-advices ()
    "Remove advices for vscroll while point movement."
    (interactive)
    (pk/vscroll-advices #'advice-remove))

  (defun pk/add-vscroll-advices ()
    "Add advices for vscroll while point movement."
    (interactive)
    (pk/vscroll-advices #'advice-add))

  (defun pk/ultra-scroll-advices ()
    "Add or remove advices for vscroll while point movement."
    (if ultra-scroll-mode
        (pk/add-vscroll-advices)
      (pk/remove-vscroll-advices)))

  :hook
  (helm-before-initialize . pk/remove-vscroll-advices)
  (helm-cleanup . pk/add-vscroll-advices)
  (ultra-scroll-mode . pk/ultra-scroll-advices)

  :custom
  ;; From Exordium init-look-and-feel to  ensure line-by-line scrolling
  (scroll-step 1)
  (scroll-conservatively 100000)

  (scroll-margin 0)
  (ultra-scroll-hide-functions '(hl-line-mode global-hl-line-mode))

  :config
  (pk/add-vscroll-advices)
  (ultra-scroll-mode))

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

(use-package gptel
  :autoload (gptel-api-key-from-auth-source)
  :custom
  ;; (gptel-model 'gemini-2.5-pro-exp-03-25)
  (gptel-model 'gpt-5)
  :config
  ;; See https://github.com/marketplace?type=models for list of models, go to
  ;; playground then get model name form Code tab if a snipped has been
  ;; generated.
  (gptel-make-openai "Github"
    :host "models.inference.ai.azure.com"
    :endpoint "/inference"
    :stream t
    :key #'gptel-api-key-from-auth-source
    :models '(gpt-5 gpt-5-mini gpt-4.1 gpt-4o))
  (gptel-make-gemini "Gemini"
    :key #'gptel-api-key-from-auth-source))


(use-package gnus-topic
  :ensure nil
  :demand nil
  :defer t
  :autoload (gnus-topic-mode))

(use-package gnus-group
  :ensure nil
  :demand nil
  :defer t
  :autoload (gnus-group-list-all-groups))

(use-package gnus
  :ensure nil
  :demand nil
  :defer t
  :functions (pk/gnus-group-list-subscribed-groups
              pk/helm-completing-read-hack-gnus-default)
  :defines (gnus-tmp-lines)
  :init
  (defun pk/gnus-group-list-subscribed-groups ()
    "List all subscribed groups with or without un-read messages."
    (interactive)
    (gnus-group-list-all-groups 5))

  (defun pk/helm-completing-read-hack-gnus-default (args)
    "Escape (some) regexps in 4th argument in ARGS for gnus commands.
This is designed to update DEF argument for
`helm-completing-read-default-handler'."
    (when-let* (((listp args))
                (def (nth 4 args))
                (name (nth 8 args))
                ((and (stringp def) (stringp name)
                      (string-prefix-p "gnus-" name))))
      (setf (nth 4 args) ;; `when-let*' doesn't bind places
            (thread-last def
                         (replace-regexp-in-string (rx "+") "\\\\+")
                         (replace-regexp-in-string (rx "*") "\\\\*")
                         (replace-regexp-in-string (rx "?") "\\\\?"))))
    args)

  (defun gnus-user-format-function-human-lines (_)
    "Convert LINES for a human consumption.
The result is always up to 4 characters long and is an approximation for
a number over a 1000."
    (pcase gnus-tmp-lines
      ((rx (any "1-9") (>= 9 (any digit)))
       "1G")
      ((and (rx (group (any "1-9") (** 0 2 (any digit))) (= 6 (any digit))) l)
       (format "%sM" (match-string 1 l)))
      ((and (rx (group (any "1-9") (** 0 2 (any digit))) (= 3 (any digit))) l)
       (format "%sk" (match-string 1 l)))
      ((and (pred stringp) l) (concat l " "))
      (l l)))

  :bind
  (:map gnus-group-mode-map
        ("o" . #'pk/gnus-group-list-subscribed-groups))
  :custom
  (gnus-use-cache t)
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date
                                (not gnus-thread-sort-by-number)))

  (gnus-thread-hide-subtree t)
  (gnus-thread-ignore-subject t)
  (gnus-not-empty-thread-mark ?\uffee)  ; ￮ half width white circle

  ;; Original was: "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n"
  ;; - added %e
  ;; - replaced %4L with %4Lu&human-lines;
  (gnus-summary-line-format "%e%U%R%z%I%(%[%4u&human-lines;: %-23,23f%]%) %s\n")
  :hook
  ;; Tree view for groups.
  (gnus-group-mode . gnus-topic-mode)
  (gnus-mode . bug-reference-mode)
  :config
  (advice-add #'helm-completing-read-default-handler
              :filter-args #'pk/helm-completing-read-hack-gnus-default))

(use-package debbugs
  :ensure t
  :demand t
  :functions (pk/debbugs-gnu-search-with-this-command)
  :init
  (use-package transient
    :demand nil
    :autoload (transient-parse-suffix
               transient-get-suffix))
  (use-package magit-section
    :ensure magit
    :demand nil
    :autoload (magit-region-values))
  (use-package helm-mode
    :ensure helm
    :demand nil
    :autoload (helm-completing-read-default-handler)
    :defines (helm-completing-read-handlers-alist))
  (use-package helm-lib
    :ensure helm-core
    :demand nil
    :autoload (helm-this-command
               helm-symbol-name))

  (use-package debbugs-gnu
    :ensure debbugs
    :demand nil
    :autoload (debbugs-gnu-pick-commits))

  (defun pk/debbugs-gnu-search-completing-read (&rest args)
                                        ; checkdoc-params: (args)
    "Allow explicitly search when completing read in `debbugs-gnu-search'.
This is done by adding an extra source that yields nil to perform search
or generates user error to abort operation.  See the
`debbugs-gnu-search''s interactive form."
    (if (equal (car-safe args) "Enter attribute: ")
        (helm :sources (list
                        (helm-build-sync-source "Action"
                          :candidates '("Do Search" "Abort")
                          :action (lambda (candidate)
                                    (when (equal "Abort" candidate)
                                      (user-error "Search aborted"))))
                        (helm-build-sync-source "Query Attributes"
                          :candidates (cadr args)))
              :buffer "*debbugs-gnu-search*")
      ;; Delegate to `helm-completing-read-default-handler'.
      (let* ((current-command (or (helm-this-command) this-command))
             (str-command (if current-command
                              (helm-symbol-name current-command)
                            "completing-read"))
             (buf-name (format "*%s*" str-command)))
        (apply #'helm-completing-read-default-handler
               (append args
                       (list str-command buf-name))))))

  (defun pk/debbugs-gnu-pick-commits-completing-read
      (prompt collection predicate require-match _intial-input hist def _inherit-input-method)
    ;; checkdoc-params: (prompt collection predicate require-match hist def)
    "Allow user to handle dynamic candidates in `debbugs-gnu-pick-commits'.
This is done by wrapping candidates (actually `debugs-gnu-completion'
table) in a lambda that keep expanding it whenever `helm-pattern'
changes."
    (let ((result
           (helm
            :sources
            (list
             (helm-build-sync-source "Bugs"
               :candidates (lambda ()
                             (let ((all-completions
                                    (completion-all-completions helm-pattern
                                                                collection
                                                                predicate
                                                                (length helm-pattern))))
                               (when (cdr (last all-completions))
                                 (setcdr (last all-completions) nil))
                               (cl-union  (mapcar #'substring-no-properties
                                                  all-completions)
                                          (ensure-list def)
                                          :test #'string=)))
               :must-match require-match
               :volatile t))
            :buffer "*debbugs-gnu-pick-commits*"
            :prompt prompt
            :history (when (symbolp hist) hist)
            :default (or (car-safe def)
                         def))))
      (when (and result hist (symbolp hist) (not (eq hist t)))
        (delete-dups (append (mapcar #'substring-no-properties
                                     (ensure-list result))
                             (symbol-value hist))))
      result))

  (defun pk/debbugs-gnu-search-with-this-command (&rest _)
    "Adjust `debbugs-gnu-search' interactive spec to work with `helm'.
Ensure `this-command' is set such that when `completing-read' calls into
`helm' it can dispatch to `pk/debbugs-gnu-search-completing-read'.  Also
handle `helm-exit-status'."
    (interactive
     (lambda (spec)
       (let ((advice (lambda (orig-fun &rest args)
                       (let ((this-command 'debbugs-gnu-search))
                         (prog1
                             (apply orig-fun args)
                           ;; Can't move abort on C-g to
                           ;; `pk/debbugs-gnu-search-completing-read' since it
                           ;; yields Command attempted to use minibuffer while
                           ;; in minibuffer and the helm doesn't go away.
                           (when (and (equal (car-safe args) "Enter attribute: ")
                                      (equal helm-exit-status 1))
                             (user-error "Search aborted")))))))
         (unwind-protect
             (progn
               (advice-add #'completing-read :around advice)
               (advice-eval-interactive-spec spec))
           (advice-remove #'completing-read advice))))))

  (defun pk/debbugs-gnu-read-commit-range-from-magit ()
    "Read commit range from a `magit' buffer.
Return commit at point or a commit range in region if it is active."
    (when-let* ((range (if-let* ((commits (magit-region-values '(commit branch) t)))
                           (progn (deactivate-mark)
                                  (format "%s^..%s" (car (last commits)) (car commits)))
                         (magit-section-case
                           ;; `debbugs-gnu-read-commit-range-from-vc-log'
                           ;; returns just a single commit in such a case but I
                           ;; don't think it is intuitive.  A message of such a
                           ;; commit is used to search for the bug reference
                           ;; (well, this one is expected) while subsequent
                           ;; patches are created with all commits leading to a
                           ;; tip of the current branch (see git format-patch
                           ;; --help).  ATM, the `debbugs-gnu-post-patch' takes
                           ;; extra argument FORMAT-PATCH-ARGS which could be
                           ;; used to format a single commit, but that would
                           ;; require an advice that would add "-1" unless
                           ;; COMMIT-RANGE is already a range.
                           (commit (let ((commit (oref it value)))
                                     (format "%s^..%s" commit commit)))))))
      (list range)))

  :hook
  (debbugs-gnu-read-commit-range . pk/debbugs-gnu-read-commit-range-from-magit)
  (bug-reference-mode . debbugs-browse-mode)
  (bug-reference-prog-mode . debbugs-browse-mode)

  :custom
  (debbugs-gnu-trunk-directory "~/gh/emacs-mirror/emacs")
  (debbugs-gnu-branch-directory "~/gh/mituharu/emacs-mac")
  (debbugs-gnu-apply-patch-prefers-magit t)
  (debbugs-gnu-default-bug-number-list
   (string-join
    (list
     (propertize "67604" 'help-echo "Motion problems with inline images")
     (propertize "77944" 'help-echo "31.0.50; M-x man on macOS fails with error")
     (propertize "78712" 'help-echo "31.0.50; definition is void in file-notify")
     (propertize "78766" 'help-echo "100-4000x redisplay slowdown with vscroll>0 and make-cursor-line-fully-visible=t")
     (propertize "79188" 'help-echo "Cannot build packages installed from VC")
     (propertize "79521" 'help-echo "31.0.50; debbugs-gnu-pick-commits no longer works")
     )
    ","))

  :config
  (add-to-list 'helm-completing-read-handlers-alist
               '(debbugs-gnu-search . pk/debbugs-gnu-search-completing-read))
  (add-to-list 'helm-completing-read-handlers-alist
               '(debbugs-gnu-pick-commits . pk/debbugs-gnu-pick-commits-completing-read))
  (advice-add #'debbugs-gnu-search
              :before #'pk/debbugs-gnu-search-with-this-command)

  (with-eval-after-load 'magit-patch
    (let ((suffix [("M-p" "debbugs pick commits" debbugs-gnu-pick-commits)]))
      (unless (equal (transient-parse-suffix 'magit-patch suffix)
                     (transient-get-suffix 'magit-patch '(-1 -1)))
        (transient-append-suffix 'magit-patch '(-1 -1) suffix)))))


(use-package log-edit
  :ensure nil
  :demand t
  :functions (pk/log-edit-magit-commit-setup
              pk/log-edit-diff-fileset)
  :init
  (use-package magit-git
    :ensure magit
    :demand nil
    :autoload (magit-changed-files
               magit-staged-files
               magit-anything-staged-p
               magit-anything-unstaged-p
               magit-gitdir))
  (use-package magit-mode
    :ensure magit
    :demand nil
    :autoload (magit-repository-local-get))
  (use-package magit-base
    :ensure magit
    :demand nil
    :autoload (magit-file-lines))
  (use-package vc
    :ensure nil
    :demand nil
    :autoload (vc-buffer-sync-fileset
               vc-diff-internal))
  (use-package vc-git
    :ensure nil
    :demand nil
    :defines (vc-git-diff-switches))

  (defun pk/log-edit-diff-fileset ()
    ;; Like in `magit-diff-1'
    (let ((staged (magit-anything-staged-p))
          (unstaged (magit-with-toplevel
                      (magit-anything-unstaged-p)))
          (command (magit-repository-local-get 'this-commit-command))
          (squash (let ((f (expand-file-name "rebase-merge/rewritten-pending"
                                             (magit-gitdir))))
                    (and (file-exists-p f) (length (magit-file-lines f))))))
      (pcase-let* ((`(,vc-git-diff-switches ,rev1 ,rev2)
                    (pcase (list staged unstaged command)
                      ((and (or
                             `(,_ ,_ magit-commit--rebase)
                             ;; TODO: below 3 cases are from autopsy
                             `(,_, _ with-editor-finish)
                             `(,_ ,_ magit-rebase-continue)
                             `(t ,_ nil))
                            (guard (integerp squash)))
                       (list "--cached" (format "HEAD~%s" squash) nil))
                      (`(,_ ,_ magit-commit-amend)
                       (list "--cached" "HEAD^" nil))
                      (`(nil nil magit-commit--allow-empty)
                       (list nil "HEAD" nil))
                      ((or `(,_ ,_ magit-commit-reword)
                           `(nil nil ,_))
                       (list nil "HEAD^" "HEAD"))
                      (`(,_ t magit-commit--all)
                       (list nil "HEAD" nil))
                      (`(nil t handle-switch-frame)
                       ;; Either --all or --allow-empty. Assume it is the former.
                       (list nil "HEAD" nil)))))
        (magit-with-toplevel
          (let* ((files (mapcar #'expand-file-name
                                (or (magit-changed-files (if rev2
                                                          (format "%s..%s" rev1 rev2)
                                                        rev1))
                                    (magit-staged-files))))
                 (fileset (list 'Git files)))
            (vc-buffer-sync-fileset fileset nil)
            (vc-diff-internal t fileset rev1 rev2))))))

  (defun pk/log-edit-magit-commit-setup ()
    (unless log-edit-vc-backend
      (setq-local log-edit-vc-backend 'Git
                  log-edit-diff-function #'pk/log-edit-diff-fileset)))

  :hook
  (log-edit-mode . pk/log-edit-magit-commit-setup))


(use-package nix-mode
  :mode "\\.nix\\'")


;; There may still be more to come,
;; e.g. https://yhetil.org/emacs-devel/87ectrl9u8.fsf@posteo.net/
(unless (fboundp 'package-report-bug) ;; Since Emacs 31
  (defun package-report-bug (desc)
    "Prepare a message to send to the maintainers of a package.
DESC must be a `package-desc' object."
    (interactive (list (package--query-desc package-alist))
                 package-menu-mode)
    (let ((maint (package-maintainers desc))
          (name (symbol-name (package-desc-name desc)))
          (pkgdir (package-desc-dir desc))
          vars)
      (when pkgdir
        (dolist-with-progress-reporter (group custom-current-group-alist)
            "Scanning for modified user options..."
          (when (and (car group)
                     (file-in-directory-p (car group) pkgdir))
            (dolist (ent (get (cdr group) 'custom-group))
              (when (and (custom-variable-p (car ent))
                         (boundp (car ent))
                         (not (eq (custom--standard-value (car ent))
                                  (default-toplevel-value (car ent)))))
                (push (car ent) vars))))))
      (dlet ((reporter-prompt-for-summary-p t))
        (reporter-submit-bug-report maint name vars)))))


(provide 'after-init)

;;; after-init.el ends here
