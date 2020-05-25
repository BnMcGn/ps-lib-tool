(in-package #:ps-lib-tool)

;;; Import manager

;;; An add on for ps-library

;;; Ps-libraries will sometimes need to import things from npm libraries for use by macros. The macros
;;; will need to find the imports. Because macros are expanded into the same namespace as user code,
;;; there is the possibility that the name used by the macro will clash with something that the user
;;; or another ps-library does in the same space.
;;;
;;; Therefore we place all imports from ps-libraries into their own namespace in a single object.
;;; Import manager supplies a few macros to make this easier.

(defparameter *import-manager-location* '_ps_lib_modules)

(ps:defpsmacro lib (name &optional package)
  `(@ ,*import-manager-location* ,(or package *package*) ,name))

(ps:defpsmacro chainl (name &rest params)
  `(chain (lib ,name) ,@params))

(ps:defpsmacro libloc (&optional package)
  `(@ ,*import-manager-location* ,(or package *package*)))

(setf
 (gethash 'import-ref ps::*macro-toplevel*)
 (gethash 'paren6:import ps::*macro-toplevel*))

(ps:defpsmacro collect-imports (&body body)
  (let ((symbols nil))
    `(macrolet
         ((paren6:import ((&rest names) module)
            (setf symbols
                  (append symbols
                          (mapcar (lambda (itm) (if (listp itm) (second itm) itm)) names)))
            `(import-ref ,names ,module)))
       ,@body
       (create6 ,@symbols))))

(ps:defpsmacro manage-imports (&body body)
  `(setf
    (libloc)
    (collect-imports
     ,@body)))
