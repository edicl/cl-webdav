;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-WEBDAV; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-webdav/handlers.lisp,v 1.13 2007/05/19 22:34:35 edi Exp $

;;; Copyright (c) 2007-2009, Dr. Edmund Weitz.  All rights reserved.

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

(defun dav-dispatcher (request &optional (resource-class *resource-class*))
  "A generic Hunchentoot dispatcher \(corresponding to the
resource class RESOURCE-CLASS) for all DAV methods.  The handler
which is returned will have *RESOURCE-CLASS* bound to
RESOURCE-CLASS.  In theory, you could use this as your dispatcher
\(which doesn't call ACCEPT-REQUEST-P first), but it's not
exported and only used internally by CREATE-DAV-DISPATCHER."
  (let ((handler (case (request-method request)
                   (:options 'options-handler)
                   (:propfind 'propfind-handler)
                   (:get 'get-handler)
                   (:head 'head-handler)
                   (:proppatch 'proppatch-handler)
                   (:put 'put-handler)
                   (:copy 'copy-handler)
                   (:move 'move-handler)
                   (:mkcol 'mkcol-handler)
                   (:delete 'delete-handler)
                   (otherwise 'not-implemented))))
    (lambda ()
      (let ((*resource-class* resource-class))
        (funcall handler)))))

(defun options-dispatcher (request)
  "A dispatcher which'll dispatch to OPTIONS-HANDLER in case of
an OPTIONS request and decline otherwise.  This is only useful if
you want to cater to Microsoft DAV clients which always
unconditionally send OPTIONS requests to the \"/\" root
resource.  Sigh..."
  (case (request-method request)
    (:options 'options-handler)))

(defgeneric create-dav-dispatcher (resource-class &optional ms-workaround-p)
  (:documentation "Creates and returns a dispatcher for the class
RESOURCE-CLASS which must be a subclass of RESOURCE.  If
MS-WORKAROUND-P is true \(which is the default), OPTIONS requests are
always handled irrespective of the results of ACCEPT-REQUEST-P - this
is needed to work around problems with some Microsoft DAV clients.")
  (:method ((resource-class standard-class) &optional (ms-workaround-p t))
   (create-dav-dispatcher (class-name resource-class) ms-workaround-p))
  (:method ((resource-class symbol) &optional (ms-workaround-p t))
   (lambda (request)
     (cond ((accept-request-p resource-class request)
            (dav-dispatcher request resource-class))
           (ms-workaround-p (options-dispatcher request))))))

(defun options-handler ()
  "The handler for OPTIONS requests.  Output is basically
determined by *ALLOWED-METHODS* and *DAV-COMPLIANCE-CLASSES*."
  (setf (header-out :allow) (format nil "~{~A~^, ~}" *allowed-methods*)
        (header-out :dav) (format nil "~{~D~^,~}" *dav-compliance-classes*)
        ;; Win2k wants this - sigh...
        (header-out :ms-author-via) "DAV")
  ;; no content
  nil)

(defun propfind-handler ()
  "The handler for PROPFIND requests.  Parses the request's
content body \(if there is one) and returns a corresponding
\"multistatus\" XML element using the methods for live and dead
properties."
  (let* ((depth-header (header-in* :depth))
         (depth-value (cond ((or (null depth-header)
                                 (string-equal depth-header "infinity")) nil)
                            ((string= depth-header "0") 0)
                            ((string= depth-header "1") 1)
                            (t (warn "Depth header is ~S." depth-header)
                               (bad-request))))
         (initial-resource (get-resource)))
    (unless (resource-exists initial-resource)
      (not-found))
    (multiple-value-bind (properties propname)
        (parse-propfind (raw-post-data :force-binary t))
      (setf (content-type*) "text/xml; charset=utf-8"
            (return-code*) +http-multi-status+)
      (let ((result
             ;; loop through the resource and its descendants until
             ;; depth limit is reached
             (loop for depth = depth-value then (if depth (1- depth) nil)
                   for resources = (list initial-resource)
                   then (and (or (null depth) (not (minusp depth)))
                             (mapcan #'resource-children resources))
                   while resources
                   nconc (loop for resource in resources
                               collect (collect-properties resource
                                                           properties
                                                           (not propname))))))
        (serialize-xmls-node (apply #'dav-node "multistatus" result))))))

(defun proppatch-handler ()
  "The handler for PROPPATCH requests.  Parses the request's
content body, modifies the dead properties as specified and
returns a corresponding \"multistatus\" XML element."
  (let ((resource (get-resource)))
    (unless (resource-exists resource)
      (not-found))
    ;; RESULTS will be a list of conses where the car is the STATUS
    ;; and the cdr is the property which was to be removed or set
    (let (results)
      ;; loop through all "actions" which are "set" or "remove" nodes
      (dolist (action (node-children
                       (parse-dav (raw-post-data :force-binary t) "propertyupdate")))
        ;; the function to apply, i.e. what to do with the property
        (let ((property-handler (if (equal (local-name action) "remove")
                                  #'remove-dead-property
                                  #'set-dead-property)))
          ;; loop through the properties which are the children of the
          ;; "prop" element within the "set" or "remove" element
          (dolist (property (node-children (first (node-children action))))
            ;; skip whitespace (which hasn't been removed as the
            ;; "spec" is :ANY)
            (cond ((whitespace-string-p property))
                  ((dav-property-function property)
                   (push (cons +http-conflict+ property) results))
                  (t (funcall property-handler resource property)
                     (push (cons +http-ok+ property) results))))))
      (setf (content-type*) "text/xml; charset=utf-8"
            (return-code*) +http-multi-status+)
      (serialize-xmls-node
       (dav-node "multistatus"
                 (apply #'dav-node "response"
                        (dav-node "href" (resource-href resource))
                        (loop for (status . property) in results
                              collect (dav-node "propstat"
                                                (dav-node "prop" (remove-content property))
                                                (dav-node "status" (status-line status))))))))))

(defun get-handler (&optional head-request-p)
  "The handler for GET requests.  Serves the contents of the
resource using SEND-CONTENT and sets up the HTTP headers
correctly.  Also doubles as handler for HEAD requests if
HEAD-REQUEST-P is true."
  (let ((resource (get-resource)))
    (unless (resource-exists resource)
      (not-found))
    (when (resource-collection-p resource)
      (forbidden))
    (let ((etag (resource-etag resource))
          (write-date (resource-write-date resource))
          (content-language (resource-content-language resource)))
      (setf (content-type*) (resource-content-type resource))
      (when etag 
        (setf (header-out :etag) etag))
      (when content-language
        (setf (header-out :content-language) content-language))
      (handle-if-modified-since write-date)
      (when (equal etag (header-in* :if-none-match))
        (setf (return-code) +http-not-modified+)
        (abort-request-handler))
      (setf (header-out :last-modified) (rfc-1123-date write-date)
            (content-length*) (resource-length resource))
      (unless head-request-p
        (send-content resource (send-headers))))))

(defun head-handler ()
  "The handler for HEAD requests - the actual work is done by
GET-HANDLER."
  (get-handler t))

(defun multi-status (results &optional (default-return-code +http-no-content+))
  "Utility function which returns a MULTISTATUS response to the
HTTP client which is based on RESULTS.  RESULTS must be a list of
conses where the cdr is the resource and the car is the
corresponding status code.  If RESULTS is NIL, not MUTILSTATUS
response will be generated and DEFAULT-RETURN-CODE will be used
instead."
  (unless results
    (setf (return-code*) default-return-code)
    (abort-request-handler))
  (setf (content-type*) "text/xml; charset=utf-8"
        (return-code*) +http-multi-status+)
  ;; use a hash table to group by status code
  (let ((status-hash (make-hash-table)))
    (loop for (status . resource) in results
          do (push resource (gethash status status-hash)))
    (let ((responses
           (loop for status being the hash-keys of status-hash
                 using (hash-value resources)
                 collect (apply #'dav-node "response"
                                `(,@(loop for resource in resources
                                          collect (dav-node "href" (resource-href resource)))
                                  ,(dav-node "status" (status-line status)))))))
      (serialize-xmls-node (apply #'dav-node "multistatus" responses)))))

(defun delete-handler ()
  "The handler for DELETE requests.  Uses REMOVE-RESOURCE* to do
the actual work."
  (let ((depth-header (header-in* :depth)))
    (unless (or (null depth-header)
                (string-equal depth-header "infinity"))
      (warn "Depth header is ~S." depth-header)
      (bad-request)))
  (let ((resource (get-resource)))
    (unless (resource-exists resource)
      (not-found))
    (multi-status (remove-resource* resource))))

(defun put-handler ()
  "The handler for PUT requests.  Uses GET-CONTENT to create a
new resource from the contents sent by the client."
  (let* ((resource (get-resource))
         (name (resource-name resource)))
    (when (or (null name)
              (whitespace-string-p name))
      (forbidden))
    (when (and (resource-exists resource)
               (resource-collection-p resource))
      (conflict))
    (let ((parent (resource-parent resource)))
      (when (or (null parent) (not (resource-exists parent)))
        (conflict)))
    (let* ((content-length-header (cdr (assoc :content-length (headers-in*))))
           (content-length (and content-length-header
                                (parse-integer content-length-header :junk-allowed t))))
      (unless content-length
        (bad-request))
      (get-content resource (raw-post-data :want-stream t) content-length))
    (resource-created resource)))

(defun copy-handler (&optional movep)
  "The handler for COPY requests which internally uses
COPY-OR-MOVE-RESOURCE* to do the actual work.  Also doubles as a
handler for MOVE requests if MOVEP is true."
  (let* ((depth-header (header-in* :depth))
         (depth-value (cond ((or (null depth-header)
                                 (string-equal depth-header "infinity")) nil)
                            ((and (string= depth-header "0")
                                  (not movep)) 0)
                            (t (warn "Depth header is ~S." depth-header)
                               (bad-request))))
         (overwrite (equal (header-in* :overwrite) "T"))
         (source (get-resource)))
    ;; note that we ignore a possible request body and thus the
    ;; "propertybehaviour" XML element for now - we just try to use
    ;; best effort to copy/move all properties
    (unless (resource-exists source)
      (not-found))
    (let ((destination-header (header-in* :destination)))
      (unless destination-header
        (warn "No 'Destination' header.")
        (bad-request))
      (when (ppcre:scan "^https?://" destination-header)
        ;; it's an absolute destination header
        (let ((uri-prefix (resource-uri-prefix source)))
          (unless (starts-with-p destination-header uri-prefix)
            ;; the URI prefix must match
            (bad-gateway))
          ;; compute destination by stripping off the prefix
          (setq destination-header
                (subseq destination-header (1- (length uri-prefix))))))
      (let* ((destination (get-resource (url-decode* destination-header)))
             (destination-exists (resource-exists destination)))
        ;; make sure we aren't creating an infinite loop
        (loop for parent = destination then (resource-parent parent)
              while (and parent (resource-exists parent))
              when (string= (resource-script-name parent)
                            (resource-script-name source))
              do (forbidden))
        (when destination-exists
          (unless overwrite
            (precondition-failed))
          ;; according to the RFC we must remove the destination first
          (when (remove-resource* destination)
            (failed-dependency)))
        (let ((results (copy-or-move-resource* source destination movep depth-value)))
          (cond (results (multi-status results))
                (destination-exists (setf (return-code*) +http-no-content+
                                          (content-type*) nil)
                                    nil)
                (t (resource-created destination))))))))

(defun move-handler ()
  "The handler for MOVE requests.  Calls COPY-HANDLER to do the
actual work."
  (copy-handler t))

(defun mkcol-handler ()
  "The handler for MKCOL requests which uses CREATE-COLLECTION
internally."
  (let ((resource (get-resource)))
    (when (resource-exists resource)
      (setf (header-out :allow)
            (format nil "~{~A~^, ~}"
                    (set-difference *allowed-methods* '(:get :head :mkcol))))
      (method-not-allowed))
    (let ((parent (resource-parent resource)))
      (unless (and parent (resource-exists parent))
        (conflict)))
    (handler-case
        (create-collection resource)
      (error (condition)
        (warn "While trying to create collection ~S: ~A"
              (resource-script-name resource) condition)
        (setf (return-code*) +http-internal-server-error+))
      (:no-error (&rest args)
        (declare (ignore args))
        (resource-created resource)))))