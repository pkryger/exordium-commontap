;;; init-vc-checkout.el --- Install packages from VC -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'package)
(require 'use-package)
(require 'vc)

(defcustom exordium-vc-checkout-alist nil
  "An alist of packages to install from VC checkout.
Each element is of a form (PACKAGE DIR), where PACKAGE is the
package name (symbol) and DIR is a directory containing a VC
checkout with the package.  When DIR is relative it will be
expanded within `user-emacs-direcory'."
  :type '(alist :key-type (symbol :tag "Package")
                :value-type (filepath :tag "Directory with checkout"))
  :group 'exordium)

(defcustom exordium-always-vc-checkout nil
  "Treat every package as though it had specified using `:exordium-vc-checkout'.
Note that this will cause already installed packages to be
overwritten with the checked out version, should they have an
entry in `exordium-vc-checkout-alist' and the directory with the
checkout exists when the corresponding `use-package' is
evaluated.  For example this can happen when Emacs is restarted
or when `use-package' form is evaluated with `eval-buffer',
`eval-last-sexp' etc."
  :type 'boolean
  :group 'exordium)

(use-package use-package-core
  :ensure nil
  :defer t
  :autoload (use-package-process-keywords))


(defun use-package-normalize/:exordium-vc-checkout (name keyword args)
                                        ; checkdoc-params: (name keyword args)
  "Normalize possible arguments to the :exordium-vc-checkout."
  (cond
   ((and (listp args) (listp (cdr args))
         (= (length args) 1))
    (let ((arg (car args)))
      (pcase arg
        ('nil nil) ; don't install when `:exordium-vc-checkout' is nil
        ('t (list name))
        ((and (pred stringp)
              (guard (< 0 (length arg))))
         (let ((path (if (file-name-absolute-p arg)
                         arg
                       (expand-file-name arg user-emacs-directory))))
           (list name path)))
        (_ (use-package-error (format "Unrecognized argument to %s. \
The keyword wants no arguments or an argument of nil, t, \
or a directory with a checkout."
                                      (symbol-name keyword)))))))
   ((not args)
    (list name)) ; install when `:exordium-vc-checkout' without any argument
   (t
    (use-package-error (concat (symbol-name keyword)
                               " wants at most one argument")))))

(defun exordium--vc-checkout-valid-p (dir)
  "Return non nil when DIR is good enough to attempt using it as a VC checkout."
  (and (stringp dir)
       (not (equal "" dir))
       (file-directory-p dir)
       (vc-responsible-backend dir t)))

(defun exordium--vc-checkout-package-delete (desc)
  "Forcibly delete package DESC and remove it from `load-path'."
  (package-delete desc 'force)
  (setq load-path (cl-remove-if
                   (lambda (dir)
                     (equal dir (package-desc-dir desc)))
                   load-path)))

(defun exordium--vc-checkout-install (name &optional dir)
  "Install the package NAME from a VC checkout.
Package is installed from DIR (if non nil) or form a directory
specified in corresponding entry in `exordium-vc-checkout-alist'.
Delete package NAME if it has been previously installed from VC
or ELPA, except for the case when the package has already been VC
installed from DIR."
  (when-let* ((dir (or dir (alist-get name exordium-vc-checkout-alist)))
              ((exordium--vc-checkout-valid-p dir)))
    (let (installed)
     (when-let* ((desc (cadr (assq name package-alist)))
                 (pkg-dir (package-desc-dir desc)))
       (pcase (package-desc-kind desc)
         ((and
           'vc
           (guard (not
                   (setq installed
                         (equal (file-truename (file-name-as-directory dir))
                                (file-truename (file-name-as-directory
                                                (package-desc-dir desc))))))))
          ;; Package has been previously installed from :vc, but checkout
          ;; appeared on a subsequent eval.  This is to avoid deleting package
          ;; when it has been already VC installed from the specified checkout
          ;; in DIR.
          (message ":exordium-vc-checkout overriding VC package %s in \
%s with checkout in %s"
                   name (package-desc-dir desc) dir)
          (exordium--vc-checkout-package-delete desc))
         ((and kind
               (guard (not (eq kind 'vc))))
          ;; Package has been previously installed from ELPA (or otherwise), but
          ;; checkout appeared on a subsequent eval.
          (message ":exordium-vc-checkout overriding %s package %s \
with checkout in %s"
                   (or kind "ELPA") name dir)
          (exordium--vc-checkout-package-delete desc))))
     (unless installed
       (package-vc-install-from-checkout dir (symbol-name name))))))

(defun use-package-handler/:exordium-vc-checkout (name _keyword arg rest state)
  "Generate code to install package NAME from a VC checkout, or do so directly.
When the `use-package' declaration is part of a byte-compiled
file, install the package during compilation; otherwise, add it
to the macro expansion and wait until runtime.  Package NAME is
VC installed from the checkout in DIR being (car ARG).  No code
is generated when ARG is nil.  The remaining arguments are as
follows:

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
    (when arg ; `:exordium-vc-checkout' is non-nil
      (if (bound-and-true-p byte-compile-current-file)
          (apply #'exordium--vc-checkout-install arg)            ; compile time
        (push `(exordium--vc-checkout-install ',(car arg) ,(cadr arg))
              body)))                                            ; runtime
    body))

(eval-after-load 'use-package-core
  '(progn
     (setf (alist-get :exordium-vc-checkout use-package-defaults)
           '((lambda (name _args)
               (list name))
             (lambda (name args)
               (and exordium-always-vc-checkout
                    (not (plist-get args :exordium-vc-checkout))))))
     (add-to-list 'use-package-keywords :exordium-vc-checkout)))



(provide 'init-vc-checkout)

;;; init-vc-checkout.el ends here
