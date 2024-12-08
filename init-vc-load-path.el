;;; init-vc-load-path.el --- Install packages from VC -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'package)
(require 'use-package)
(require 'vc)

(defcustom exordium-use-package-vc-load-paths
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

(defun use-package-normalize/:exordium-vc-load-path (_name keyword args)
                                        ; checkdoc-params: (keyword args)
  "Normalize possible arguments to the :exordium-vc."
  ;; TODO: handle empty string? (and in defaults)
  (use-package-only-one (symbol-name keyword) args
    #'use-package-normalize-paths))

(defun exordium--vc-load-path-valid-p (dir)
  "Return non nil when DIR is good enough for checkout."
  (and (stringp dir)
       (not (equal "" dir))
       (file-directory-p dir)
       (vc-responsible-backend dir)))

(defun exordium--vc-load-path-package-delete (name keyword dir)
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

(defun use-package-handler/:exordium-vc-load-path (name keyword arg rest state)
                                        ; checkdoc-params: (keyword rest state)
  "VC Install package NAME from an existing checkout directory.
Direcotory is (car ARG).  Package VC installation is done only if
the directory exists.

Processing is done after :load-path, such
that the directory can be prepended to :load-path's arguments to
force :vc to eventually call `package-vc-install-from-checkout'
with the directory."
  (if-let* ((dir (car arg))
            ((exordium--vc-load-path-valid-p dir)))
      (use-package-concat
       ;; :load-path has been done, let's do as it does.
       `((eval-and-compile (add-to-list 'load-path ,dir)))

       ;;
       (if (bound-and-true-p byte-compile-current-file)
           (funcall #'exordium--vc-load-path-package-delete name keyword dir)
         `((exordium--vc-load-path-package-delete ',name ,keyword ,dir)))

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
  '(unless (memq :exordium-vc-load-path use-package-keywords)
     (push :exordium-vc-load-path
           (nthcdr
            (1+ (cl-position :load-path use-package-keywords))
            use-package-keywords))))


;; TODO: unlikely as we want to be affecting :load-path
;; (eval-after-load 'use-package-core
;;   '(add-to-list 'use-package-keywords :exordium-vc-load-path))

(quote
 (macroexpand
  (quote
   (use-package difftastic
     :exordium-vc-load-path "/Users/pkryger/gh/pkryger/difftastic.el"
     :vc t
     :load-path "/foo/bar"))))

(quote
 (package-desc-archive (cadr (assq 'delight package-alist))))

(quote
 (macroexpand
  (quote
   (use-package delight
     :exordium-vc-load-path "/Users/pkryger/gh/savannah/delight"))))

(defvar exordium--vc-load-path-orig-ensure-func
  (nth 2 (assq :ensure use-package-defaults)))

(defun exordium--vc-load-path-default-gate (func)
  "Create a wrapper around FUNC to gate setting of default value.
Wrapper is designed to be used around default gate function for
keyword :ensure in `use-pakcage-defaults'.  It prevents setting
up the defult for keyword :ensure when there's an existing
directory for a package NAME."
  (lambda (name args)
    (and (not (when-let* ((dir (or
                                (alist-get name
                                           exordium-use-package-vc-load-paths)
                                (car (plist-get args :exordium-vc-load-path)))))
                (exordium--vc-load-path-valid-p dir)))
         ;; Do as `use-package-process-keywords' does.
         (if (and func (functionp func))
             (funcall func name args)
           (eval func)))))

(eval-after-load 'use-package-core
  '(setf (nth 2 (assq :ensure use-package-defaults))
         (exordium--vc-load-path-default-gate
          exordium--vc-load-path-orig-ensure-func)))

(let ((use-package-always-ensure t)
      (exordium-use-package-vc-load-paths '((difftastic . "/Users/pkryger/gh/pkryger/difftastic.el"))))
  (not
   (funcall (exordium--vc-load-path-default-gate
             exordium--vc-load-path-orig-ensure-func)
            'difftastic nil)))

(let ((use-package-always-ensure t)
      (exordium-use-package-vc-load-paths '((difftastic . "/not-an-existing-path"))))
  (funcall (exordium--vc-load-path-default-gate
            exordium--vc-load-path-orig-ensure-func)
           'difftastic nil))

(let ((use-package-always-ensure t)
      (exordium-use-package-vc-load-paths '((difftastic . t))))
  (funcall (exordium--vc-load-path-default-gate
            exordium--vc-load-path-orig-ensure-func)
           'difftastic nil))

(let ((use-package-always-ensure t)
      (exordium-use-package-vc-load-paths nil))
  (not
   (funcall (exordium--vc-load-path-default-gate
             exordium--vc-load-path-orig-ensure-func)
            'difftastic '(:exordium-vc-load-path
                          "/Users/pkryger/gh/pkryger/difftastic.el"))))

(let ((use-package-always-ensure t)
      (exordium-use-package-vc-load-paths nil))
   (funcall (exordium--vc-load-path-default-gate
                       exordium--vc-load-path-orig-ensure-func)
                      'difftastic '(:exordium-vc-load-path
                                    "/not-an-existing-path")))

(let ((use-package-always-ensure t)
      (exordium-use-package-vc-load-paths nil))
  (funcall (exordium--vc-load-path-default-gate
            exordium--vc-load-path-orig-ensure-func)
           'difftastic '(:exordium-vc-load-path
                         t)))

(defvar pk/orig-use-package-defaults use-package-defaults)

(eval-after-load 'use-package-core
  '(setf (alist-get :exordium-vc-load-path use-package-defaults)
    '((lambda (name _args)
        (use-package-normalize/:exordium-vc-load-path
         name
         :exordium-vc-load-path
         (list (alist-get name exordium-use-package-vc-load-paths))))
      (lambda (name args)
        (and (when-let* ((dir (alist-get name
                                         exordium-use-package-vc-load-paths))
                         ((exordium--vc-load-path-valid-p dir)))
             (not (plist-member args :exordium-vc-load-path)))))))



(provide 'init-vc-load-path)

;;; init-vc-load-path.el ends here
