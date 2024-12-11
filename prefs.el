;;; prefs.el --- commontap  -*- lexical-binding: t; -*-


;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (unless (featurep 'init-require)
    (load (file-name-concat (locate-user-emacs-file "modules") "init-require"))))
(exordium-require 'init-prefs)

(setq exordium-always-vc-checkout t)
(let ((workspace (or (getenv "GITHUB_WORKSPACE")
                     (getenv "HOME"))))
  (setq exordium-vc-checkout-alist
        `(;; ("jinx"
          ;;  . ,(file-name-concat workspace "gh" "minad" "jinx"))
          ("difftastic"
           . ,(file-name-concat workspace "gh" "pkryger" "difftastic.el"))
          ("basic-stats"
           . ,(file-name-concat workspace "gh" "pkryger" "basic-stats"))
          ("emacs-toml"
           . ,(file-name-concat workspace "gh" "gongo" "emacs-toml"))
          ("ultra-scroll-mac"
           . ,(file-name-concat workspace "gh" "jdtsmith" "ultra-scroll-mac")))))


(setq exordium-preferred-frame-width  nil
      exordium-preferred-frame-height nil)
(setq exordium-preferred-fonts
      '(("Fira Code" . 130)
        ("Hack" . 130)
        ("DejaVu Sans Mono for Powerline" . 130)
        ("M+ 1mn" . 130)
        ("M+ 1m" . 130)
        ("Monaco" . 130)))

(defcustom exordium-preferred-variable-fonts
  '(("Fira Sans"  . 125))
  ;; TODO: come up with a list of reasonable fonts
  "TODO: List of preferred fonts/sizes to use, in decreasing order of
preference. We will use the first one that is available on the
local machine. It is a list of pairs (font-name . font-size). If
nil, we don't set any font. Note that you can get the list of
available font names by evaluating (font-family-list)."
  ;; TODO::group 'exordium
  :type  'sexp
  :group 'exordium)

(setq exordium-preferred-variable-fonts
      '(("Fira Sans" . 125)))

;; (custom-set-faces '(variable-pitch ((t (:family "Fira Sans" :height 135)))))

;; (set-face-attribute 'default nil
;;                     :family font
;;                     :height size
;;                     :weight 'normal)
(setq exordium-helm-completion-style 'orderless)
(setq exordium-desktop t)
(setq exordium-helm-everywhere t)
(setq exordium-theme nil)
(setq exordium-fci-mode :prog)
(setq exordium-fci-use-dashes :two)
(setq exordium-highlight-symbol t)
(setq exordium-enable-c++11-keywords :modern)
(setq exordium-complete-mode :company)
(setq exordium-display-line-numbers t)
(setq exordium-inhibit-line-numbers-star-buffers t)
(setq exordium-inhibit-line-numbers-buffer-size (* 512 1024))
(setq exordium-height-plus-1 1.075)
(setq exordium-height-plus-2 1.15)
(setq exordium-height-plus-3 1.225)
(setq exordium-height-plus-4 1.3)
(setq exordium-no-org-babel-confirm t)
(setq exordium-rtags-syntax-checker nil)
(setq exordium-lsp-mode-enable nil)
(setq exordium-treesit-modes-enable t)

(provide 'prefs)

;;; prefs.el ends here
