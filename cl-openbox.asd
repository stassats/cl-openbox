;;; -*- Mode: Lisp -*-

(asdf:defsystem #:cl-openbox
  :serial t
  :depends-on (drakma cxml-stp)
  :components ((:file "packages")
               (:file "cl-openbox")))
