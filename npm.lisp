;;;; npm.lisp

(in-package #:ps-lib-tool)

;;; "ps-lib-tool" goes here. Hacks and cwdebuggingglory await!

;;FIXME: These shouldn't be hard coded.
(defvar *npm-executable-path* #p"/usr/bin/npm")
(defvar *node-executable-path* #p"/usr/bin/nodejs")
(defvar *browserify-path* #p"/usr/local/bin/browserify")
(defvar *cache-path* (asdf:system-relative-pathname
                      'ps-lib-tool "cache" :type :directory))


(defun package-json-path (path)
  (merge-pathnames path "package.json"))

(defun npm-executable-path ()
  (unless (uiop:file-exists-p *npm-executable-path*)
    (error
     (format nil "Npm not found at ~a. Please install npm or specify an executable with the *npm-executable-path* variable." *npm-executable-path*)))
  *npm-executable-path*)

(defun node-executable-path ()
  (unless (uiop:file-exists-p *node-executable-path*)
    (error
     (format nil "Nodejs not found at ~a. Please install node or specify an executable with the *node-executable-path* variable." *node-executable-path*)))
  *node-executable-path*)

(defun browserify-executable-path ()
  (unless (uiop:file-exists-p *browserify-path*)
    (error
     (format nil
             "Browserify not found at ~a. Please install browserify or specify an executable with the *browserify-path* variable."
             *browserify-path*)))
  *browserify-path*)

(defun load-package-json-at-path (path)
  (when (uiop:file-exists-p (package-json-path path))
    (json:decode-json-from-source (package-json-path path))))

(defun save-package-json-at-path (path data)
  "Saves the supplied data in the cache directory's package.json file. Uses cl-json to convert to json format. See npm documentation for file format."
  (with-open-file (fh (package-json-path path)
                      :direction :output :if-exists :overwrite
                      :if-does-not-exist :create)
    (json:encode-json data fh)))

(defun npm-command (&rest command)
  (uiop:with-current-directory (*cache-path*)
    (uiop/run-program:run-program
     (list* (npm-executable-path) command)
     :output :lines)))

;;FIXME Should present errors to user. Should offer to install missing modules, probably with a restart.
;; Note: this might not be our final user interface.
(defun build-browserify-bundle (destination modules &key working-dir toplevel-file)
  (uiop:with-current-directory ((or working-dir *cache-path*))
    (uiop/run-program:run-program
     `(,(browserify-executable-path)
       ,@(mapcan (lambda (module) (list "-r" module))
                 modules)
       ,@(when toplevel-file (list (princ-to-string toplevel-file))))
     :output destination
     :error-output t)))

(defun install-save (target-dir package &key version)
  "Causes the package information to be written to the dependencies section of package.json in the target dir."
  (let ((*cache-path* target-dir))
    (npm-command
     "install"
     (if version (format nil "~a@~a" package version) package)
     "-save")))


;;; Dubious stuff

(defun npm-version ()
  "Returns the version of the npm binary in use."
  (car (npm-command "-v")))

(defun update ()
  "Updates all npm packages in the cache."
  (npm-command "update"))

(defun install (package-name &key version global)
  "Installs named package in the cache directory. If global keyword is set attempts to install package globally."
  (let ((package-name (if version (format nil "~a@~a" package-name version) package-name)))
    (apply #'npm-command `("install" ,@(when global (list "-g")) ,package-name))))


(defun npm-list ()
  (handler-bind ((uiop:subprocess-error
                   (lambda (e) (declare (ignore e)) (invoke-restart 'continue))))
    (uiop:with-current-directory ("/home/ben/quicklisp/local-projects/wf-static/")
      (cl-json:decode-json-from-string
       (uiop:run-program (list (ps-lib-tool::npm-executable-path) "list" "--json")
                         :output :string)))))

(defun npm-installed-packages (&key list-data)
  (cl-utilities:collecting
    (labels ((proc (itm)
               (when (assoc :version (cdr itm))
                 (cl-utilities:collect (cons (car itm) (gadgets:assoc-cdr :version (cdr itm)))))
               (when (assoc :dependencies (cdr itm))
                 (dolist (dep (gadgets:assoc-cdr :dependencies (cdr itm)))
                   (proc dep)))))
      (dolist (itm (gadgets:assoc-cdr :dependencies (or list-data (npm-list))))
        (proc itm)))))

(defun report-problems (packlist)
  )

(defun up-to-date-p (installed requested)
  (when installed
    (if requested
        (semver:version<= (semver:read-version-from-string requested)
                          (semver:read-version-from-string installed)))))

(defun ensure-packages-installed (packlist)
  (let ((installed (hu:collecting-hash-table ()
                       (dolist (pack (npm-list))
                         (hu:collect (car pack) (second pack))))))
    (dolist (rpack packlist)
      (alexandria:if-let ((version (gethash (car rpack) installed)))
        (unless (up-to-date-p version (second rpack))
          (install (car rpack) :version (second rpack)))
        (install (car rpack) :version (second rpack))))))

(defun uninstall (package-name &key save global)
  (apply #'npm-command
         `("uninstall"
           ,@(when save "--save") ,@(when global "-g") ,package-name)))



