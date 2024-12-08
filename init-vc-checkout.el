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

(use-package use-package-core
  :ensure nil
  :defer t
  :autoload (use-package-vc-install
             use-package-only-one
             use-package-normalize-paths
             use-package-process-keywords))

(defun use-package-normalize/:exordium-vc-checkout (_name keyword args)
                                        ; checkdoc-params: (keyword args)
  "Normalize possible arguments to the :exordium-vc."
  ;; TODO: handle empty string? (and in defaults)
  (use-package-only-one (symbol-name keyword) args
    #'use-package-normalize-paths))

(defun exordium--vc-checkout-valid-p (dir)
  "Return non nil when DIR is good enough for checkout."
  (and (stringp dir)
       (not (equal "" dir))
       (file-directory-p dir)
       (vc-responsible-backend dir)))

(defun exordium--vc-checkout-package-delete (name keyword dir)
                                        ; checkdoc-params: (keyword)
  "Delete package NAME if it has been previously installed from VC or ELPA.
Avoid deletion when the package has been installed from VC into DIR."
  (when-let* ((desc (cadr (assq name package-alist))))
    (pcase (package-desc-kind desc)
           ((and 'vc
                 (guard (not
                         (equal (file-truename (file-name-as-directory dir))
                                (file-truename (package-desc-dir desc))))))
            ;; Package has been previously installed from :vc, but checkout
            ;; appeared on a subsequent eval.  This is to avoid deleting
            ;; package when it has been already VC installed from a desired
            ;; checkout.
            (message "%s overriding VC package %s in %s with checkout in %s"
                     keyword name (package-desc-dir desc) dir)
            (package-delete desc 'force))
           ((and (pred (not (lambda (kind)
                              (eq kind 'vc))))
                 kind)
            ;; Package has been previously installed from ELPA (or otherwise),
            ;; but checkout appeared on a subsequent eval.
            (message "%s overriding %s package %s with checkout in %s"
                     keyword (or kind "ELPA") name dir)
            (package-delete desc 'force)))))

(defun use-package-handler/:exordium-vc-checkout (name keyword arg rest state)
                                        ; checkdoc-params: (keyword rest state)
  "VC Install package NAME from an existing checkout directory.
Direcotory is (car ARG).  Package VC installation is done only if
the directory exists.

Processing is done after :load-path, such
that the directory can be prepended to :load-path's arguments to
force :vc to eventually call `package-vc-install-from-checkout'
with the directory."
  (if-let* ((dir (car arg))
            ((exordium--vc-checkout-valid-p dir)))
      (use-package-concat
       ;; :load-path has been done, let's do as it does.
       `((eval-and-compile (add-to-list 'load-path ,dir)))

       ;;
       (if (bound-and-true-p byte-compile-current-file)
           (funcall #'exordium--vc-checkout-package-delete name keyword dir)
         `((exordium--vc-checkout-package-delete ',name ,keyword ,dir)))

       ;; TODO: built-ins are `package-installed-p' too, so :vc won't install them
       (unless (plist-member rest :vc)
         (if (bound-and-true-p byte-compile-current-file)
             (funcall #'use-package-vc-install (list name) dir) ; compile time
           `((use-package-vc-install ',(list name) ,dir))))     ; runtime

       (use-package-process-keywords name rest
         (if (member dir (plist-get state :load-path))
             state
           (use-package-plist-cons state :load-path dir))))
    (use-package-process-keywords name rest state)))

(eval-after-load 'use-package-core
  '(unless (memq :exordium-vc-checkout use-package-keywords)
     (push :exordium-vc-checkout
           (nthcdr
            (1+ (cl-position :load-path use-package-keywords))
            use-package-keywords))))


;; TODO: unlikely as we want to be affecting :load-path
;; (eval-after-load 'use-package-core
;;   '(add-to-list 'use-package-keywords :exordium-vc-checkout))

(quote
 (macroexpand
  (quote
   (use-package difftastic
     :exordium-vc-checkout "/Users/pkryger/gh/pkryger/difftastic.el"
     :vc t
     :load-path "/foo/bar"))))

(quote
 (package-desc-archive (cadr (assq 'delight package-alist))))

(quote
 (macroexpand
  (quote
   (use-package delight
     :exordium-vc-checkout "/Users/pkryger/gh/savannah/delight"))))

(defvar exordium--vc-checkout-orig-ensure-func
  (nth 2 (assq :ensure use-package-defaults)))

(defun exordium--vc-checkout-default-gate (func)
  "Create a wrapper around FUNC to gate setting of default value.
Wrapper is designed to be used around default gate function for
keyword :ensure in `use-pakcage-defaults'.  It prevents setting
up the defult for keyword :ensure when there's an existing
directory for a package NAME."
  (lambda (name args)
    (and (not (when-let* ((dir (or
                                (alist-get name
                                           exordium-vc-checkout-alist)
                                (car (plist-get args :exordium-vc-checkout)))))
                (exordium--vc-checkout-valid-p dir)))
         ;; Do as `use-package-process-keywords' does.
         (if (and func (functionp func))
             (funcall func name args)
           (eval func)))))

(eval-after-load 'use-package-core
  '(setf (nth 2 (assq :ensure use-package-defaults))
         (exordium--vc-checkout-default-gate
          exordium--vc-checkout-orig-ensure-func)))

(let ((use-package-always-ensure t)
      (exordium-vc-checkout-alist '((difftastic . "/Users/pkryger/gh/pkryger/difftastic.el"))))
  (not
   (funcall (exordium--vc-checkout-default-gate
             exordium--vc-checkout-orig-ensure-func)
            'difftastic nil)))

(let ((use-package-always-ensure t)
      (exordium-vc-checkout-alist '((difftastic . "/not-an-existing-path"))))
  (funcall (exordium--vc-checkout-default-gate
            exordium--vc-checkout-orig-ensure-func)
           'difftastic nil))

(let ((use-package-always-ensure t)
      (exordium-vc-checkout-alist '((difftastic . t))))
  (funcall (exordium--vc-checkout-default-gate
            exordium--vc-checkout-orig-ensure-func)
           'difftastic nil))

(let ((use-package-always-ensure t)
      (exordium-vc-checkout-alist nil))
  (not
   (funcall (exordium--vc-checkout-default-gate
             exordium--vc-checkout-orig-ensure-func)
            'difftastic '(:exordium-vc-checkout
                          "/Users/pkryger/gh/pkryger/difftastic.el"))))

(let ((use-package-always-ensure t)
      (exordium-vc-checkout-alist nil))
   (funcall (exordium--vc-checkout-default-gate
                       exordium--vc-checkout-orig-ensure-func)
                      'difftastic '(:exordium-vc-checkout
                                    "/not-an-existing-path")))

(let ((use-package-always-ensure t)
      (exordium-vc-checkout-alist nil))
  (funcall (exordium--vc-checkout-default-gate
            exordium--vc-checkout-orig-ensure-func)
           'difftastic '(:exordium-vc-checkout
                         t)))

(defvar pk/orig-use-package-defaults use-package-defaults)

(eval-after-load 'use-package-core
  '(setf (alist-get :exordium-vc-checkout use-package-defaults)
    '((lambda (name _args)
        (use-package-normalize/:exordium-vc-checkout
         name
         :exordium-vc-checkout
         (list (alist-get name exordium-vc-checkout-alist))))
      (lambda (name args)
        (and (when-let* ((dir (alist-get name
                                         exordium-vc-checkout-alist))
                         ((exordium--vc-checkout-valid-p dir)))
             (not (plist-member args :exordium-vc-checkout)))))))



(provide 'init-vc-checkout)

;;; init-vc-checkout.el ends here
