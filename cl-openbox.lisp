;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:cl-openbox)

(defvar *api-key* "q6z66hyvukccv2r4lnc0j5umh87j30bh")
(defvar *auth-token* nil)

(defun rest-request (parameters)
  (drakma:http-request
   "https://www.box.net/api/1.0/rest"
   :parameters `(("api_key" . ,*api-key*)
                 ,@(when *auth-token*
                         `(("auth_token" . ,*auth-token*)))
                 ,@parameters)))

(defun find-child (local-name xml)
  (stp:find-child local-name xml :key #'stp:local-name :test #'equal))

(defun find-child-path (xml &rest local-names)
  (loop for name in local-names
        for node = (find-child name xml) then (find-child name node)
        while node
        finally (return node)))

(defun parse-xml (xml)
  (cxml:parse xml (stp:make-builder)))

(defun get-response (xml)
  (find-child "response" (parse-xml xml)))

(defun get-ticket ()
  (stp:string-value
   (find-child "ticket"
               (get-response (rest-request `(("action" . "get_ticket")))))))

(defun get-auth-token (ticket)
  (stp:string-value
   (find-child "auth_token"
               (get-response (rest-request `(("action" . "get_auth_token")
                                             ("ticket" . ,ticket)))))))

(defun authenticate ()
  (let ((ticket (get-ticket)))
    (format t "Please visit https://www.box.net/api/1.0/auth/~a and authenticate."
            ticket)
    (when (y-or-n-p "Press y afterwards.")
      (get-auth-token ticket))))

(defun parse-directory (xml)
  (stp:with-attributes ((id "id")
                        (name "name")
                        (file-name "file_name")) xml
    (list (or name file-name)
          id)))

(defun parse-directories (xml)
  (let ((folder (find-child-path xml "tree" "folder")))
    (list :directories
          (stp:map-children 'list
                            'parse-directory
                            (find-child "folders" folder))
          :files
          (stp:map-children 'list
                            'parse-directory
                            (find-child "files" folder)))))

(defun list-directories ()
  (parse-directories
   (get-response
    (rest-request `(("action" . "get_account_tree")
                    ("folder_id" . "0")
                    ("params[]" . "nozip")
                    ("params[]" . "simple")
                    ("params[]" . "onelevel"))))))

(defun upload-file (file &key (directory "0") share)
  (drakma:http-request
   (format nil "https://upload.box.net/api/1.0/upload/~a/~a" *auth-token* directory)
   :method :post
   :parameters `(("share" . ,(if share "1" "0"))
                 ("file" . ,(pathname file)))))

(defun download-file (file-id save-into)
  (let ((file (drakma:http-request
               (format nil "https://www.box.net/api/1.0/download/~a/~a"
                       *auth-token* file-id))))
    (when file
      (with-open-file (stream save-into :direction :output
                              :element-type '(unsigned-byte 8))
        (write-sequence file stream)))
    t))
