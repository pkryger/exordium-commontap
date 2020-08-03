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
  (setq ispell-extra-args (list
                           "--sug-mode=ultra"
                           "--camel-case"))
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

(use-package 'deft
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
  (setq desktop-files-not-to-save
        (rx-let ((path (+ (or alnum digit "." "/" "-" "_" "~"))))
          (rx (or (seq string-start "/" (zero-or-more (not (any "/" ":"))) ":")
                  (seq "(ftp)" string-end)
                  (seq string-start path "/emacs/" path "/lisp/" path
                       ".el.gz" string-end)
                  (seq string-start path "/.emacs.d/elpa/" path
                       ".el" string-end))))))

(require 'which-key)
(which-key-mode)
(diminish 'which-key-mode)

(use-package python
  :init
  (setq python-shell-interpreter "python3"))

(use-package py-autopep8
  :ensure t
  :hook (python-mode . py-autopep8-enable-on-save))

(use-package python-pytest
  :ensure t
  :bind (:map python-mode-map
              ("C-c C-y" . python-pytest-popup))
  :init
  (setq python-pytest-executable
        (concat python-shell-interpreter " -m pytest")))

(use-package pyvenv
  :ensure t
  :init
  (defun pk/pyvenv-activate (&optional prefix)
    (interactive "P")
    (if-let* ((dir (file-name-as-directory
                    (concat
                     (file-name-as-directory (projectile-project-root))
                     "venv")))
              (directory
               (when (and (not prefix)
                          (file-exists-p (concat
                                          (file-name-as-directory
                                           (concat dir "bin"))
                                          "activate")))
                 dir)))
        (pyvenv-activate directory)
      (call-interactively #'pyvenv-activate)))
  :config
  (add-hook 'pyvenv-post-activate-hooks #'pyvenv-restart-python)
  :bind (:map python-mode-map
              ("C-c C-n" . pk/pyvenv-activate)))

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

;; Load R as well
(use-package ess
  :ensure t)
(use-package ess-r-mode
  :demand t)
(org-babel-do-load-languages
 'org-babel-load-languages
 (append org-babel-load-languages
         '((R . t))))

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
      "<-" ";;" "\\n" "fl" "Fl" "Tl" "www" ".."
      ;; org-mode ballots -> they are unicode chars, not glyphs
      ;; "[ ]" "[X]" "[-]"
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

(add-hook 'git-commit-mode-hook 'turn-on-auto-fill)
(require 'forge)
(add-hook 'forge-post-mode-hook #'(lambda () (set-fill-column 100000)))
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

;; Tab navigation just like in iTerm
(defun pk/mac-goto-tab (n)
  "Go to N-th open tab."
  (interactive
   (list (completing-read "Go to Tab: "
                          (or (seq-map-indexed
                               (lambda (_ idx)
                                 (number-to-string idx))
                               (mac-frame-tab-group-property nil :frames))
                              '("0"))
                          nil t)))
  (let ((frame (nth
                (if (stringp n)
                    (string-to-number n)
                  n)
                (mac-frame-tab-group-property nil :frames))))
    (when frame
      (mac-set-frame-tab-group-property nil :selected-frame frame))))

(dolist (num '(0 1 2 3 4 5 6 7 8 9))
  (global-set-key (kbd (concat "H-"
                               (number-to-string (mod (1+ num) 10))))
                  `(lambda () (interactive) (pk/mac-goto-tab ,num))))


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
