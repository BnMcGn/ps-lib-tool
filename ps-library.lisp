;;;; ps-library

(in-package #:cl-npm)

(defvar *ps-packages* (make-hash-table))

(defun def-ps-package (name &key ps-imports npm-imports code export)
  "Define a parenscript package. Ps-packages are used to bundle parenscript code and specify dependencies. Dependencies can be other ps-packages or npm libraries. Ps-packages can be built into javascript programmatically or can be exported as npm libraries."

  (setf (gethash name *ps-packages*)
        (list
         :ps-imports ps-imports
         :npm-imports (check-npm-imports npm-imports)
         :code code
         ;;FIXME: export might be obsolete. Use a node lib.
         :export export)))

(defun check-npm-imports (imps)
  (unless
      (every (lambda (x) (or (stringp x) (and (listp x) (every #'stringp x))))
             imps)
    (error "Invalid item in npm-imports section"))
  imps)

(defun find-all-required-ps-packages (requiring-package-symbol)
  "Find all of the ps-packages that will be needed by the specified package. Will NOT search for packages in the depends-on section of .asd files. Dependencies must be explicitly stated in the ps-imports section of the def-ps-package form."
  (let ((stor (make-hash-table))
        (work (getf (gethash requiring-package-symbol *ps-packages*) :ps-imports))
        (new-work nil))
    (loop while work
          do (progn
               (dolist (item work)
                 (unless (gethash item stor)
                   (push item new-work)
                   (setf (gethash item stor) (gethash item *ps-packages*))))
               (setf work new-work)))
    stor))

(defun get-all-node-requirements (&rest ps-packages)
  (apply #'append
         (maphash (lambda (key val)
                    (declare (ignore key))
                    (getf val :npm-imports))
                  (apply #'append (mapcar #'find-all-required-ps-packages ps-packages)))))

(defun strip-version-string (vstring)
  ;;FIXME: gadgets
  (nth-value 1 (gadgets:divide-on-true (lambda (x) (<= (char-int #\0) (char-int x) (char-int #\9)))
                                       vstring)))

(defun compare-versions (ver1 ver2)
  "Returns the larger"
  (if ver1
      (if ver2
          (if (semver:version<=
               (semver:read-version-from-string (strip-version-string ver1))
               (semver:read-version-from-string (strip-version-string ver2)))
              ver2 ver1)
          ver1)
      ver2))

(defun merge-node-requirements (reqs)
  "Requirements come in as a list with possible duplicates and/or different version specifications. Reduce to list of same first appearance order with highest requested version for each library."
  (let ((stor (make-hash-table :test #'equal))
        (accum))
    (dolist (itm reqs)
      (let ((key (if (listp itm) (car itm) itm))
            (version (if (listp itm) (second itm) nil)))
        (when version (assert (stringp version)))
        (push key accum)
        (setf (gethash key stor)
              (if-let ((val (gethash key stor)))
                (compare-versions val version)
                version))))
    (mapcar
     (lambda (key)
       (let ((val (gethash key stor)))
         (if val (list key val) key)))
     (gadgets:ordered-unique (nreverse accum)))))





