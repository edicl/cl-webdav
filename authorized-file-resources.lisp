;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-WEBDAV; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-webdav/authorized-file-resources.lisp,v 1.7 2007/04/18 19:21:00 edi Exp $

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

(defclass authorized-file-resource (file-resource)
  ()
  (:documentation "A subclass of FILE-RESOURCE representing file
resources which are associated with a certain user."))

(defmethod create-dav-dispatcher :around ((resource-class (eql 'authorized-file-resource))
                                          &optional ms-workaround-p)
  "This :AROUND method wraps the normal DAV dispatcher with one
that checks basic HTTP authentication first."
  (let ((normal-dispatcher (call-next-method)))
    (lambda (request)
      (or (options-dispatcher request)
          (let ((handler (funcall normal-dispatcher request)))
            (when handler
              (multiple-value-bind (user password)
                  (authorization request)
                (cond ((and user
                            ;; we allow only certain characters
                            (ppcre:scan "^[a-zA-Z0-9_-]+$" user)
                            ;; password must match user name, test is
                            ;; case-insensitive
                            (equalp user password))
                       handler)
                      (t #'require-authorization)))))))))

(defmethod file-resource-base-path-namestring ((resource-class (eql 'authorized-file-resource)))
  "We compute the base path by attaching the downcased user name
to the base path returned for plain file resources.  We also
create the directory if necessary."
  (namestring
   (truename
    (ensure-directories-exist
     (format nil "~A~A/"
             (file-resource-base-path-namestring 'file-resource)
             ;; note that this assumes that we're within a HTTP request
             ;; which is OK
             (string-downcase (authorization)))))))

(defmethod accept-request-p ((resource-class (eql 'authorized-file-resource)) request)
  "We just use the method for FILE-RESOURCE."
  (accept-request-p 'file-resource request))
