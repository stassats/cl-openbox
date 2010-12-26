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

(defun list-files (&optional (folder-id "0"))
  (parse-folders
   (action "get_account_tree"
           `("folder_id" . ,folder-id)
           '("params[]" . "nozip")
           '("params[]" . "simple")
           '("params[]" . "onelevel"))))

(defun upload (content &key name (folder "0") share)
  "content may be a file-name, a sequence of octets, a binary input stream."
  (let* ((content (if (stringp content)
                      (pathname content)
                      content))
         (name (or name
                   (typecase content
                     (pathname (file-namestring content))
                     (file-stream (pathname content))
                     (t (error "Unable to determine name for ~a"
                               (type-of content)))))))
    (parse-files
     (find-child "files"
                 (get-response
                  (drakma:http-request
                   (format nil "https://upload.box.net/api/1.0/upload/~a/~a" 
                           *auth-token* folder)
                   :method :post
                   :content-length t
                   :parameters `(("file" ,content :filename ,name)
                                 ,@(if share '(("share" . "1"))))))))))

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

(defun parse-create-folder (xml)
  (let ((id (find-child-path xml "folder" "folder_id")))
    (when id
      (stp:string-value id))))

(defun create-folder (name &key (parent-id "0") share)
  (parse-create-folder
   (action "create_folder"
           (cons "name" name)
           (cons "parent_id" parent-id)
           (cons "share"
                 (if share
                     "1"
                     "0")))))
