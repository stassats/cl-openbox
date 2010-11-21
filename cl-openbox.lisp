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

(defun parse-xml (xml)
  (cxml:parse xml (stp:make-builder)))

(defun get-response (xml)
  (find-child "response" (parse-xml xml)))

(defun action (action-name &rest parameters)
  (get-response (rest-request `(("action" . ,action-name)
                                ,@(remove nil parameters)))))

(defun find-child (local-name xml)
  (stp:find-child local-name xml :key #'stp:local-name :test #'equal))

(defun find-child-path (xml &rest local-names)
  (loop for name in local-names
        for node = (find-child name xml) then (find-child name node)
        while node
        finally (return node)))

(defun get-ticket ()
  (stp:string-value
   (find-child "ticket" (action "get_ticket"))))

(defun get-auth-token (ticket)
  (stp:string-value
   (find-child "auth_token"
               (action "get_auth_token" `("ticket" . ,ticket)))))

(defun authenticate ()
  (let ((ticket (get-ticket)))
    (format t "Visit https://www.box.net/api/1.0/auth/~a and authenticate."
            ticket)
    (when (y-or-n-p "Press y afterwards.")
      (setf *auth-token* (get-auth-token ticket)))))

(defun parse-file (xml)
  (stp:with-attributes ((id "id")
                        (name "name")
                        (file-name "file_name")) xml
    (list (or name file-name)
          id)))

(defun parse-files (xml)
  (stp:map-children 'list 'parse-file xml))

(defun parse-folders (xml)
  (let* ((folder (find-child-path xml "tree" "folder"))
         (folders (find-child "folders" folder))
         (files (find-child "files" folder)))
    (list (when folders
            (list :folders (parse-files folders)))
          (when files
            (list :files (parse-files files))))))

(defun list-files ()
  (parse-folders
   (action "get_account_tree"
           '("folder_id" . "0")
           '("params[]" . "nozip")
           '("params[]" . "simple")
           '("params[]" . "onelevel"))))

(defun upload (file &key (folder "0") share)
  (parse-files
   (find-child "files"
               (get-response
                (drakma:http-request
                 (format nil "https://upload.box.net/api/1.0/upload/~a/~a" 
                         *auth-token* folder)
                 :method :post
                 :content-length t
                 :parameters `(("file" ,(pathname file))
                               ,@(if share '(("share" . "1")))))))))

(defun download (file-id save-into)
  (let ((file (drakma:http-request
               (format nil "https://www.box.net/api/1.0/download/~a/~a"
                       *auth-token* file-id))))
    (when file
      (with-open-file (stream save-into :direction :output
                              :element-type '(unsigned-byte 8))
        (write-sequence file stream)))
    t))

(defun delete (id &key folder)
  (action "delete"
          (cons "target_id" id)
          (cons "target"
                (if folder
                    "folder"
                    "file"))))

(defun rename (id new-name &key folder)
  (action "rename"
          (cons "target_id" id)
          (cons "new_name" new-name)
          (cons "target"
                (if folder
                    "folder"
                    "file"))))

(defun create-folder (name &key (parent-id "0") share)
  (action "create_folder"
          (cons "name" name)
          (cons "parent_id" parent-id)
          (cons "share"
                (if share
                    "1"
                    "0"))))
