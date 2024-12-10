;;; init-vc-checkout.el --- Install packages from VC -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'package)
(require 'use-package)
(require 'vc)

(defcustom exordium-vc-checkout-alist
  '((difftastic . "/Users/pkryger/gh/pkryger/difftastic.el"))
  "TODO:."
  :type '(alist :key-type (symbol :tag "Package")
                :value-type (choice (filepath :tag "Checkout location")
                                    (plist :tag "use-package :vc args")))
  :group 'exordium)

(defcustom exordium-always-vc-checkout t
  "TODO:."
  :type 'boolean
  :group 'exordium)

(use-package use-package-core
  :ensure nil
  :defer t
  :autoload (use-package-process-keywords))


(defun use-package-normalize/:exordium-vc-checkout (name keyword args)
                                        ; checkdoc-params: (name keyword args)
  "Normalize possible arguments to the :exordium-vc-checkout."
  (if (and (listp args) (listp (cdr args))
           (= (length args) 1))
      (let ((arg (car args)))
        (pcase arg
          ((or 'nil 't) (list name))
          ((pred stringp)
           (let ((path (if (file-name-absolute-p arg)
                           arg
                         (expand-file-name arg user-emacs-directory))))
             (list name path)))
          (_ (use-package-error (format "Unrecognized argument to %s.\
The keyword wants an argument of nil, t, or a directory with a checkout."
                                        (symbol-name keyword))))))
    (use-package-error (concat (symbol-name keyword)
                               " wants only one argument"))))

(defun exordium--vc-checkout-valid-p (dir)
  "Return non nil when DIR is good enough for checkout."
  (and (stringp dir)
       (not (equal "" dir))
       (file-directory-p dir)
       (vc-responsible-backend dir)))

(defun exordium--vc-checkout-package-delete (desc)
  "Forcibly delete package DESC and remove it from `load-path'."
  (package-delete desc 'force)
  (setq load-path (cl-remove-if
                   (lambda (dir)
                     (equal dir (package-desc-dir desc)))
                   load-path)))

(defun exordium--vc-checkout-install (name dir)
  "Install the package NAME from a VC checkout.
Package is installed from DIR (if non nil) or form a directory
specified in corresponding entry in `exordium-vc-checkout-alist'.
Delete package NAME if it has been previously installed from VC
or ELPA.  Avoid deletion when the package has been installed from
VC into DIR."
  (when-let* ((dir (or dir (alist-get name exordium-vc-checkout-alist)))
              ((exordium--vc-checkout-valid-p dir))
              (desc (cadr (assq name package-alist))))
    (pcase (package-desc-kind desc)
           ((and 'vc
                 (guard (not
                         (equal (file-truename (file-name-as-directory dir))
                                (file-truename (file-name-as-directory
                                                (package-desc-dir desc)))))))
            ;; Package has been previously installed from :vc, but checkout
            ;; appeared on a subsequent eval.  This is to avoid deleting
            ;; package when it has been already VC installed from a desired
            ;; checkout.
            (message ":exordium-vc-checkout overriding VC package %s in \
%s with checkout in %s"
                     name (package-desc-dir desc) dir)
            (exordium--vc-checkout-package-delete desc))
           ((and kind
                 (guard (not (eq kind 'vc))))
            ;; Package has been previously installed from ELPA (or otherwise),
            ;; but checkout appeared on a subsequent eval.
            (message ":exordium-vc-checkout overriding %s package %s \
with checkout in %s"
                     (or kind "ELPA") name dir)
            (exordium--vc-checkout-package-delete desc)))
    (package-vc-install-from-checkout dir (symbol-name name))))


(defun use-package-handler/:exordium-vc-checkout (name _keyword arg rest state)
  "Generate code to install package NAME from a VC checkout, or do so directly.
When the `use-package' declaration is part of a byte-compiled
file, install the package during compilation; otherwise, add it
to the macro expansion and wait until runtime.  Package NAME is
VC installed from the checkout in DIR being (car ARG).  The
remaining arguments are as follows:

_KEYWORD is ignored.

ARG is the normalized input to the `:exordium-vc-checkout'
keyword, as returned by the
`use-package-normalize/:exordium-vc-checkout' function.

REST is a plist of other (following) keywords and their
arguments, each having already been normalized by the respective
function.

STATE is a plist of any state that keywords processed before
`:exordium-vc-checkout' (see `use-package-keywords') may have
accumulated.

Also see the Info node `(use-package) Creating an extension'."
  (let ((body (use-package-process-keywords name rest state)))
    (if (bound-and-true-p byte-compile-current-file)
        (funcall #'use-package-vc-install arg)       ; compile time
      (push `(use-package-vc-install ',arg) body))   ; runtime
    body))

(eval-after-load 'use-package-core
  '(progn
     (setf (alist-get :exordium-vc-checkout use-package-defaults)
           '((lambda (name _args)
               (list name))
           exordium-always-vc-checkout))
     (add-to-list 'use-package-keywords :exordium-vc-checkout)))


;; Tests

(quote
 (macroexpand
  (quote
   (use-package delight
     :exordium-vc-checkout "/Users/pkryger/gh/savannah/delight"))))



(provide 'init-vc-checkout)

;;; init-vc-checkout.el ends here
