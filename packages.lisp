;;; -*- Mode: Lisp -*-

(defpackage #:cl-openbox
  (:use #:cl)
  (:shadow #:delete)
  (:export
   #:*api-key*
   #:*auth-token*
   #:authenticate
   #:download 
   #:delete
   #:rename
   #:list-files
   #:create-folder
   #:upload))
