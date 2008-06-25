;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-WEBDAV; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-webdav/file-resources.lisp,v 1.7 2007/04/18 19:21:00 edi Exp $

;;; Copyright (c) 2007-2008, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-webdav)

(defclass file-resource (resource)
  ((real-path :initarg :real-path
              :accessor real-path
              :documentation "The pathname of the resource within
the local file system."))
  (:documentation "A subclass of RESOURCE representing resources
which are mapped to a subtree of the local file system."))

(defun compute-real-path (script-name)
  "Computes the \"real path\" of a file resource from the
request's script name."
  ;; we basically strip off the base URI and replace it with the base
  ;; path
  (let* ((uri-suffix (subseq script-name
                             (length (file-resource-base-uri *resource-class*))))
         (real-path
           (pathname
            (concatenate 'string
                         (file-resource-base-path-namestring *resource-class*)
                         (if (starts-with-p uri-suffix "/")
                           (subseq uri-suffix 1)
                           uri-suffix)))))
    ;; maybe convert to "directory notation" if the file exists
    (or (fad:file-exists-p real-path) real-path)))

(defmethod initialize-instance :after ((resource file-resource) &rest initargs)
  "When a file resource is created, either the real path or the
script name is known.  This :AFTER method sets the other slot."
  (declare (ignore initargs))
  (cond ((slot-boundp resource 'real-path)
         (setf (resource-script-name resource)
                 (format nil "~A/~A"
                         (file-resource-base-uri (class-of resource))
                         (resource-display-name resource))))
        (t (setf (real-path resource)
                   (compute-real-path (resource-script-name resource))))))

(defmethod resource-exists ((resource file-resource))
  "A file resource exists iff the corresponding file exists."
  (fad:file-exists-p (real-path resource)))

(defmethod resource-collection-p ((resource file-resource))
  "A file resource is a collection iff the corresponding file is
a directory."
  (fad:directory-pathname-p (real-path resource)))

(defmethod resource-children ((resource file-resource))
  "The children of a \(collection) file resource are the contents
of the corresponding directory in the file system."
  (when (and (resource-collection-p resource)
             (resource-exists resource))
    (loop for real-path in (fad:list-directory (real-path resource))
          collect (make-instance 'file-resource :real-path real-path))))

(defmethod resource-parent ((resource file-resource))
  "To determine the parent of a file resource we \"walk up\" the
script name and map the result to a pathname in the file system."
  (ppcre:register-groups-bind (parent-script-name)
      ("^(.*/)[^/]+/?$" (resource-script-name resource))
    (let ((parent-real-path (ignore-errors (compute-real-path parent-script-name))))
      (when parent-real-path
        (make-instance 'file-resource :real-path parent-real-path)))))

(defmethod resource-write-date ((resource file-resource))
  "As the write date of a resource we return the write date of
the corresponding file \(or the current time in case we can't
determine the file's write date)."
  (or (file-write-date (real-path resource))
      (get-universal-time)))

(defmethod resource-length ((resource file-resource))
  "The length of a file resource is the length of the
corresponding file in octets."
  ;; we know it's not a directory
  (with-open-file (in (real-path resource) :element-type '(unsigned-byte 8))
    (file-length in)))

(defmethod resource-content-type ((resource file-resource))
  "We use Hunchentoot's MIME-TYPE function to determine the
resource's content type based on the type component of its
pathname."
  (or (mime-type (real-path resource)) "application/octet-stream"))

(defmethod resource-display-name ((resource file-resource))
  "The display name is basically the real path with the base path
stripped off.  But we also make sure to convert Windows backslashes to
Unix slashes."
  (ppcre:regex-replace-all "\\\\"
                           (subseq (namestring (real-path resource))
                                   (length (file-resource-base-path-namestring
                                            (class-of resource))))
                           "/"))

(defmethod send-content ((resource file-resource) stream)
  "To implement this method, we simply open the corresponding
file for reading and send its contents \(as chunks of octets) to
the stream."
  (with-open-file (file (real-path resource)
                        :direction :input
                        :element-type '(unsigned-byte 8))
    (loop with buf = (make-array +buffer-length+ :element-type '(unsigned-byte 8))
          for pos = (read-sequence buf file)
          until (zerop pos)
          do (write-sequence buf stream :end pos)
             (finish-output stream))))

(defmethod get-content ((resource file-resource) stream length)
  "To implement this method, we simply open the corresponding
file for writing and read its contents \(as chunks of octets)
from the stream."
  (with-open-file (file (real-path resource)
                        :direction :output
                        :if-exists :supersede
                        :element-type '(unsigned-byte 8))
    (loop with buf = (make-array +buffer-length+ :element-type '(unsigned-byte 8))
          for amount from length downto 0 by +buffer-length+
          for pos = (read-sequence buf stream :end (min +buffer-length+ amount))
          do (write-sequence buf file :end pos)
             (finish-output file))))

(defmethod remove-resource ((resource file-resource))
  "A file resource is removed by removing the corresponding file
in the file system."
  (let ((real-path (real-path resource)))
    (cond ((resource-collection-p resource)
           (when(fad:list-directory real-path)
             (error "Directory ~S is not empty." real-path))
           (fad:delete-directory-and-files real-path))
          (t (delete-file real-path)))))

(defmethod move-resource ((source file-resource) (destination file-resource))
  "A file resource is moved by moving its counterpart in the file
system \(using the Common Lisp function RENAME-FILE)."
  (rename-file (real-path source) (real-path destination)))

(defmethod copy-resource ((source file-resource) (destination file-resource))
  "A file resource is moved by copying its counterpart in the
file system \(using CL-FAD's function COPY-FILE).  Collections
\(directories) are \"copied\" simply by creating a new directory
with the same name in the destination location."
  (cond ((resource-collection-p source)
         (create-collection destination))
        (t (fad:copy-file (real-path source) (real-path destination)))))

(defmethod create-collection (resource)
  "A collection is created by creating the corresponding
directory in the file system \(using the Common Lisp function
ENSURE-DIRECTORIES-EXIST."
  (ensure-directories-exist
   (fad:pathname-as-directory (real-path resource))))

(defmethod accept-request-p ((resource-class (eql 'file-resource)) request)
  "A request is accepted if it starts with the base URI."
  (let ((script-name (script-name request)))
    ;; guard against attackers who try to walk up the directory and
    ;; out of the tree they're allowed to see
    (and (not (ppcre:scan "(?:^|/)\\.\\.(?:$|/)" script-name))
         (starts-with-p script-name (file-resource-base-uri resource-class)))))

(defgeneric file-resource-base-uri (resource-class)
  (:documentation "This generic function is called for subclasses of
FILE-RESOURCE to determine the base URI that's currently being used,
i.e. the prefix the script name of a resource's URI must have in order
to be valid.  \(In other words: this URI represents the top-level
collection of the DAV server.)  The function must return a string
which starts with a slash and does /not/ end with a slash and is
correctly URL-encoded.  You can specialize this function \(either on
the class or on the name of the class) if you want.

The default method returns the current value of
*FILE-RESOURCE-BASE-URI*.")
  (:method ((resource-class standard-class))
    (file-resource-base-uri (class-name resource-class)))
  (:method ((resource-class symbol))
    *file-resource-base-uri*))

(defgeneric file-resource-base-path-namestring (resource-class)
  (:documentation "This generic function is called for subclasses of
FILE-RESOURCE to determine the base pathname that's currently being
used, i.e. the part of the filesystem where the files served by the
DAV server are stored.  The function must return the namestring of the
truename of an absolute pathname denoting a directory, specifically it
must return a string starting and ending with slashes.  \(Note: This
should work on Windows as well.)  You can specialize this function
\(either on the class or on the name of the class) if you want.

The default method returns the current value of
*FILE-RESOURCE-BASE-PATH-NAMESTRING*.")
  (:method ((resource-class standard-class))
    (file-resource-base-path-namestring (class-name resource-class)))
  (:method ((resource-class symbol))
    *file-resource-base-path-namestring*))

;;; The methods dealing with properties are like the default methods
;;; except that they use the namestring of the "real path" as the hash
;;; key instead of the script name (because the same script name can
;;; potentially designate more than one resource depending on other
;;; parts of the HTTP request)

(defmethod get-dead-properties ((resource file-resource))
  (retrieve-properties (namestring (real-path resource))))

(defmethod remove-dead-property ((resource file-resource) property)
  (store-properties (namestring (real-path resource))
                    (remove property (get-dead-properties resource)
                            :test #'property-equal)))

(defmethod set-dead-property ((resource file-resource) property)
  (store-properties (namestring (real-path resource))
                    (cons property
                          (remove property (get-dead-properties resource)
                                  :test #'property-equal))))

(defmethod remove-dead-properties ((resource file-resource))
  (remove-properties (namestring (real-path resource))))

(defmethod move-dead-properties ((source file-resource) (destination file-resource))
  (move-properties (namestring (real-path source))
                   (namestring (real-path destination))))

(defmethod copy-dead-properties ((source file-resource) (destination file-resource))
  (copy-properties (namestring (real-path source))
                   (namestring (real-path destination))))

