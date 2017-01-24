;;;; cl-npm.asd

(asdf:defsystem #:cl-npm
  :description "A simple wrapper for npm and webpack."
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "MIT"
  :depends-on (#:uiop
               #:external-program
               #:cl-json)
  :serial t
  :components ((:file "package")
               (:file "ps-library")
               (:file "npm")))

