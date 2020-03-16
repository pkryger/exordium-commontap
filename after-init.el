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
(require 'flyspell)
(require 'ispell)
(setq flyspell-issue-message-flag nil)
(when (executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args (list
        "--sug-mode=ultra"
        "--camel-case")))
(setq ispell-dictionary "british")
(add-hook 'git-commit-mode-hook 'turn-on-auto-fill)
(add-hook 'git-commit-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(require 'helm-flyspell)
(defun pk/flyspell-jump-and-correct-word (event)
  (interactive "P")
  (when-let ((cursor-pos (mouse-position))
             (line (cddr cursor-pos))
             (col  (cadr cursor-pos))
             (p (save-excursion
                  (goto-char (window-start))
                  (forward-line line)
                  (if (> (- (line-end-position) (line-beginning-position)) col)
                      (progn  (move-to-column col) (1- (point)))
                    nil))))
    (goto-char p)
    (flyspell-correct-word-before-point event p)))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [mouse-2] nil)
     (define-key flyspell-mouse-map [H-mouse-1] 'pk/flyspell-jump-and-correct-word)
     (define-key flyspell-mode-map flyspell-auto-correct-binding 'helm-flyspell-correct)
     (define-key flyspell-mode-map [(control ?\,)] nil)
     (define-key flyspell-mode-map [(control ?\.)] nil)))


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
(setq deft-extensions '("org" "txt" "md"))
(setq deft-default-extension "org")
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-auto-save-interval 0)

(require 'ace-window)
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

(require 'python)
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
(require 'helm-swoop)
(eval-after-load "helm-swoop"
  '(progn
     (define-key helm-swoop-edit-map (kbd "C-c C-c") 'helm-swoop--edit-complete)
     (define-key helm-swoop-edit-map (kbd "C-c C-k") 'helm-swoop--edit-cancel)
     (define-key helm-swoop-edit-map (kbd "C-c C-q C-k") 'helm-swoop--edit-delete-all-lines)))

;; Diminish some modes
(diminish 'eldoc-mode)
(diminish 'auto-revert-mode)
(diminish 'flyspell-mode)
(diminish 'undo-tree-mode)

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

;; Use python3 for org
(setq org-babel-python-command "python3")
;; Don't ask for confirmation for code blocks (rather use :eval no)
(setq org-confirm-babel-evaluate nil)
;; Make tabs work nativly in org mode's src blocks
(setq org-src-tab-acts-natively t)

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
    ;; "[ ]" "[X]"
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
of `pk/mac-auto-operator-composition-characters' if the font
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
        (let ((char-strings-alist))
          (mapc (lambda (string)
                  (cl-callf append
                      (alist-get (string-to-char string) char-strings-alist)
                    (list (if (< (length string) 1)
                              ""
                            (substring string 1)))))
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
(pk/mac-auto-operator-composition-mode)

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
;; itself(the small buffer underneath) and not the buffers you compare. Which
;; is probably a preferred solution.
(add-hook 'ediff-mode-hook
          (lambda ()
            (setq auto-composition-mode nil)))


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

;; a couple statistical goodies
(defun pk/quartile (sequence quartile &optional method)
  "Return a given QUARTILE for the specified SEQUENCE.
The value is returned according to Method 1 (`:exclude') from Wiki:
`https://en.wikipedia.org/wiki/Quartile'.

The optional METHOD can be `:include' to use Method 2 instead.

Return nil unless one of:
- QUARTILE is one of 1, 2, or 3,
- SEQUENCE length is >=1 and QUARTILE is 2,
- SEQUENCE length is >=2 and QUARTILE is one of 1 or 3."
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

(defun pk/median (sequence)
  "Return a median for the specified SEQUENCE."
  (pk/quartile sequence 2))

(defun pk/five-nums (sequence &optional method)
  "Return a list consisting of (min q1 med q3 max) for the specified SEQUENCE.
The optional METHOD is the same as in `pk/quartile'.  When some values cannot be
calculated they are set to nil."
  (list (when sequence (seq-min sequence))
        (pk/quartile sequence 1 method)
        (pk/median sequence)
        (pk/quartile sequence 3 method)
        (when sequence (seq-max sequence))))


;;
