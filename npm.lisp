;;;; cl-npm.lisp

(in-package #:cl-npm)

;;; "cl-npm" goes here. Hacks and cwdebuggingglory await!

;;FIXME: These shouldn't be hard coded.
(defvar *npm-executable-path* #p"/usr/bin/npm")
(defvar *node-executable-path* #p"/usr/bin/nodejs")
(defvar *browserify-path* #p"/usr/local/bin/browserify")
(defvar *cache-path* (asdf:system-relative-pathname
                      'cl-npm "cache" :type :directory))


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

(defun package-list ()
  (mapcar (lambda (itm) (split-sequence:split-sequence
                         #\@
                         (gadgets:last-car (split-sequence:split-sequence #\  itm))))
          (cdr (npm-command "list"))))

(defun up-to-date-p (installed requested)
  (when installed
    (if requested
        (semver:version<= (semver:read-version-from-string requested)
                          (semver:read-version-from-string installed)))))

(defun ensure-packages-installed (packlist)
  (let ((installed (hu:collecting-hash-table ()
                       (dolist (pack (package-list))
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



