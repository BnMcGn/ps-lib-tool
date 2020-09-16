;;;; ps-lib-tool.asd

(asdf:defsystem #:ps-lib-tool
  :description "Utilities for building parenscript libraries, including a project generator for parenscript-based node libraries."
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "MIT"
  :depends-on (#:uiop
               #:external-program
               #:cl-json
               #:gadgets
               #:alexandria
               #:cl-semver
               #:parenscript
               #:paren6)
  :serial t
  :components ((:file "package")
               (:file "npm")
               (:file "ps-package")
               (:file "import-manager")
               (:file "node-library")))

