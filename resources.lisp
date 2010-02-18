;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-WEBDAV; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-webdav/resources.lisp,v 1.12 2007/04/18 19:21:00 edi Exp $

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

(defclass resource ()
  ((script-name :initarg :script-name
                :accessor resource-script-name
                :documentation "This slot holds the script name
\(see HUNCHENTOOT:SCRIPT-NAME) that was used to create the
resource.  For objects you create yourself, you must provide a
meaningful value that can be used to access the resource."))
  (:documentation "This is the base class you'll have to subclass
if you want to create your own custom DAV server.  Each object of
this class represents one resource on the server and most of the
time these objects are created by the server using only
the :SCRIPT-NAME initarg.  If you need more initialization to
happen, write an :AFTER method for INITIALIZE-INSTANCE.

See the file `file-resources.lisp' for an example of a subclass
of RESOURCE."))

;;; The generic functions which /must/ be specialized

(defgeneric resource-exists (resource)
  (:documentation "This function must return a true value if the
resource RESOURCE exists on the server and NIL otherwise.  You
must specialize this generic function for your own classes."))

(defgeneric resource-children (resource)
  (:documentation "This function must return a list of all
children of RESOURCE \(which themselves are RESOURCE objects).
You must specialize this generic function for your own
classes."))

(defgeneric resource-parent (resource)
  (:documentation "This function must return a RESOURCE object
which is the parent resource of RESOURCE or NIL if there is no
parent.  You must specialize this generic function for your own
classes."))

(defgeneric resource-collection-p (resource)
  (:documentation "This function must return a true value if the
resource RESOURCE is a collection.  You must specialize this
generic function for your own classes."))

(defgeneric resource-write-date (resource)
  (:documentation "This function must return a universal time
denoting the time the resource RESOURCE was last modified.  You
must specialize this generic function for your own classes."))

(defgeneric resource-length (resource)
  (:documentation "This function must return an integer denoting
the length of the resource RESOURCE in octets.  You must
specialize this generic function for your own classes."))

(defgeneric resource-display-name (resource)
  (:documentation "This function must return a string which,
according to the WebDAV RFC, \"provides a name for the resource
that is suitable for presentation to a user.\"  You must
specialize this generic function for your own classes."))

(defgeneric send-content (resource stream)
  (:documentation "This function is called for GET requests and
must send the complete contents of the \(non-collection) resource
RESOURCE to the \(flexi) stream STREAM."))

(defgeneric get-content (resource stream length)
  (:documentation "This function is called for PUT requests and
must read LENGTH octets of data from the \(flexi) stream STREAM
and store them in a place appropriate for the resource
RESOURCE."))

(defgeneric remove-resource (resource)
  (:documentation "This function must completely remove the
resource RESOURCE.  It doesn't have to deal with dead properties,
and it can assume that RESOURCE doesn't have children in case
it's a collection."))

(defgeneric move-resource (source destination)
  (:documentation "This function must \"move\" the \(contents of
the) resource SOURCE in such a way that it can in the future be
accessed as DESTINATION.  It doesn't have to deal with dead
properties, and it can assume that SOURCE doesn't have children
in case it's a collection."))

(defgeneric copy-resource (source destination)
  (:documentation "This function must \"copy\" the \(contents of
the) resource SOURCE in such a way that the copy can in the
future be accessed as DESTINATION.  It doesn't have to deal with
dead properties, and it can assume that SOURCE doesn't have
children in case it's a collection."))

(defgeneric create-collection (resource)
  (:documentation "This function must create a collection
resource that in the future can be accessed as RESOURCE."))

(defgeneric accept-request-p (resource-class request)
  (:documentation "This must be a function which accepts a
Hunchentoot request object REQUEST and returns a generalized
boolean denoting whether REQUEST denotes a resource the DAV
server wants to handle.  Usually, you'll want to look at the
script name of the request or something like that - see the class
FILE-RESOURCE for an example.

Note that you specialize this function on the resource /class/
and not on the resource.")
  (:method ((resource-class standard-class) script-name)
   (accept-request-p (class-name resource-class) script-name)))

;;; The generic functions which have default methods and thus don't
;;; necessarily need to be specialized

(defgeneric resource-creation-date (resource)
  (:documentation "This function must return a universal time
denoting the time the resource RESOURCE was created.  There's a
default method which returns RESOURCE-WRITE-DATE, but most likely
you'll want to specialize this for you own classes.")
  (:method (resource)
   (resource-write-date resource)))

(defgeneric resource-content-type (resource)
  (:documentation "This function must return a string denoting
the MIME type of the resource RESOURCE.  It will only be called
if RESOURCE is /not/ a collection.  There's a default method
which always returns \"application/octet-stream\", but most
likely you'll want to specialize this for your own classes.")
  (:method (resource) "application/octet-stream"))

(defgeneric resource-content-language (resource)
  (:documentation "This function should return either NIL or a
language tag as defined in section 14.13 of RFC 2068.  If the
value returned by this function is not NIL, it will also be used
as the `Content-Language' header returned for GET requests.
There's a default method which always returns NIL.")
  (:method (resource)))

(defgeneric resource-source (resource)
  (:documentation "This function should return either NIL or a
DAV \"source\" XML node \(structured as an XMLS node) that,
according to the WebDAV RFC, \"identifies the resource that
contains the unprocessed source of the link's source.\" There's a
default method which always returns NIL.")
  (:method (resource)))

(defgeneric resource-etag (resource)
  (:documentation "This function should return an ETag for the
resource RESOURCE or NIL.  If the value returned by this function
is not NIL, it will also be used as the `ETag' header returned
for GET requests.  There's a default method which synthesizes a
value based on the script name and the write date of the
resource, and in most cases you probably don't need to specialize
this function.")
  (:method (resource)
   (md5-hex (format nil "~A-~A"
                    (get-last-modified resource)
                    (resource-script-name resource)))))

(defgeneric resource-type (resource)
  (:documentation "This function should return either NIL or a
DAV \"resourcetype\" XML node \(structured as an XMLS node) that,
according to the WebDAV RFC, \"specifies the nature of the
resource.\"  There's a default method which returns something
fitting for collections and NIL otherwise, and in most cases you
probably don't need to specialize this function.")
  (:method (resource)
    (when (resource-collection-p resource)
      (dav-node "resourcetype" (dav-node "collection")))))

(defgeneric resource-uri-prefix (resource)
  (:documentation "This function must return a string which is
the part of a resource's HTTP or HTTPS URI that comprises the
scheme, the host, and the port and ends with a slash - something
like \"http://localhost:4242/\" or \"https://www.lisp.org/\".

The default method synthesizes this from the information
Hunchentoot provides and usually you only have to write your own
method if you're sitting behind a proxy.")
  (:method (resource)
    (format nil "http~:[~;s~]://~A~@[:~A~]/"
            (acceptor-ssl-p *acceptor*)
            (ppcre:regex-replace ":\\d+$" (acceptor-address *acceptor*) "")
            (acceptor-port *acceptor*))))

(defgeneric get-dead-properties (resource)
  (:documentation "This function must return all dead properties
of the resource RESOURCE as a list of XML elements structured as
XMLS nodes.  There's a default method but you should definitely
specialize this for production servers.")
  (:method (resource)
   (retrieve-properties (resource-script-name resource))))

(defgeneric remove-dead-property (resource property)
  (:documentation "This function must remove the currently stored
dead property designated by PROPERTY \(an XMLS node) of the
resource RESOURCE.  There's a default method but you should
definitely specialize this for production servers.")
  (:method (resource property)
   (store-properties (resource-script-name resource)
                     (remove property (get-dead-properties resource)
                             :test #'property-equal))))

(defgeneric set-dead-property (resource property)
  (:documentation "This function must replace the currently
stored dead property designated by PROPERTY \(an XMLS node) of
the resource RESOURCE with PROPERTY, i.e. PROPERTY doubles as the
property itself and as the property designator.  There's a
default method but you should definitely specialize this for
production servers.")
  (:method (resource property)
   (store-properties (resource-script-name resource)
                     (cons property
                           (remove property (get-dead-properties resource)
                                   :test #'property-equal)))))

(defgeneric remove-dead-properties (resource)
  (:documentation "This function must remove all dead properties
of the resource RESOURCE.  There's a default method but you
should definitely specialize this for production servers.")
  (:method (resource)
   (remove-properties (resource-script-name resource))))

(defgeneric move-dead-properties (source destination)
  (:documentation "This function must move all dead properties of
the resource SOURCE to the resource DESTINATION.  There's a
default method but you should definitely specialize this for
production servers.")
  (:method (source destination)
   (move-properties (resource-script-name source)
                    (resource-script-name destination))))

(defgeneric copy-dead-properties (source destination)
  (:documentation "This function must copy all dead properties of
the resource SOURCE to the resource DESTINATION.  There's a
default method but you should definitely specialize this for
production servers.")
  (:method (source destination)
   (copy-properties (resource-script-name source)
                    (resource-script-name destination))))

;;; Internal functionality

(defun resource-href (resource)
  "Returns a URL-encoded version of the resource's script name for use
in HREF elements in property XML."
  (format nil "~:[/~;~:*~{~A~^/~}~]"
          (mapcar (lambda (string)
                    (url-encode string +utf-8+))
                  (ppcre:split "/" (resource-script-name resource)))))

(defun resource-name (resource)
  "Retrieves and returns the \"name part\" of the script name of
RESOURCE, i.e. the last non-empty string behind a slash.  Note
that the result can be NIL.  This is a bit similar to
CL:FILE-NAMESTRING."
  (first (last (ppcre:split "/" (resource-script-name resource)))))

(defun get-last-modified (resource)
  "This is the function that is called for the
\"getlastmodified\" property.  It returns the result of
RESOURCE-WRITE-DATE as an RFC 1123 string within a DAV XML node."
  (let ((node (dav-node "getlastmodified" (rfc-1123-date (resource-write-date resource)))))
    (push '(("dt" . "urn:uuid:c2f41010-65b3-11d1-a29f-00aa00c14882/") "dateTime.rfc1123")
          (node-attributes node))
    node))

(defun creation-date (resource)
  "This is the function that is called for the \"creationdate\"
property.  It returns the result of RESOURCE-CREATION-DATE as an
ISO 8601 string within a DAV XML node."
  (let ((node (dav-node "creationdate" (iso-8601-date (resource-creation-date resource)))))
    (push '(("dt" . "urn:uuid:c2f41010-65b3-11d1-a29f-00aa00c14882/") "dateTime.tz")
          (node-attributes node))
    node))

(defun get-content-length (resource)
  "This is the function that is called for the
\"getcontentlength\" property.  It simply returns the result of
RESOURCE-LENGTH as a string."
  (unless (resource-collection-p resource)
    (format nil "~D" (resource-length resource))))

(defun get-content-type (resource)
  "This is the function that is called for the \"getcontenttype\"
property.  It simply returns the result of RESOURCE-CONTENT-TYPE
for non-collections and \"httpd/unix-directory\" for
collections."
  (cond ((resource-collection-p resource) "httpd/unix-directory")
        (t (resource-content-type resource))))

(defun remove-resource* (resource)
  "Removes the resource RESOURCE and \(if necessary) its children
using REMOVE-RESOURCE.  Returns a list of conses where the car is
an HTTP return code and the cdr is the corresponding resource for
exceptional situations encountered during the process."
  (unless (resource-exists resource)
    (return-from remove-resource*
      (list (cons +http-not-found+ resource))))
  (when (resource-collection-p resource)
    ;; try to remove the children first
    (let ((child-results
            (loop for child in (resource-children resource)
                  nconc (remove-resource* child))))
      (when child-results
        ;; stop recursion if something went wrong deeper down in the
        ;; hierarchy
        (return-from remove-resource* child-results))))
  ;; remove the dead properties first
  (remove-dead-properties resource)
  (handler-case
    (remove-resource resource)
    (error (condition)
      (warn "While trying to delete ~S: ~A"
            (resource-script-name resource) condition)
      (list (cons +http-internal-server-error+ resource)))
    (:no-error (&rest args)
      (declare (ignore args))
      nil)))

(defun copy-or-move-resource* (source destination movep depth)
  "Copies or moves \(depending on the generalized boolean MOVEP)
the resource denoted by SOURCE to \(the resource denoted by)
DESTINATION.  If DEPTH is NIL, recurses down to the children \(if
any) as well.  Returns a list of conses where the car is an HTTP
return code and the cdr is the corresponding \(source) resource
for exceptional situations encountered during the process."
  (unless (resource-exists source)
    (return-from copy-or-move-resource*
      (list (cons +http-not-found+ source))))
  ;; take care of dead properties
  (funcall (if movep #'move-dead-properties #'copy-dead-properties)
           source destination)
  (let (results)
    (handler-case
        (funcall (if movep #'move-resource #'copy-resource)
                 source destination)
      (error (condition)
        (warn "While trying to ~:[copy~;move~] from ~S to ~S: ~A"
              movep
              (resource-script-name source)
              (resource-script-name destination)
              condition)
        (push (cons +http-internal-server-error+ source) results)))
    (cond ((and (resource-collection-p source)
                (null depth)
                ;; only recurse if there weren't any errors
                (null results))
           (loop for source-child in (resource-children source)
                 for destination-child = (get-resource
                                          ;; synthesize script name for new child
                                          (format nil "~A~A"
                                                      (resource-script-name destination)
                                                      (resource-name source-child)))
                 nconc (copy-or-move-resource* source-child destination-child depth movep)))
          (t results))))

(defgeneric create-resource (resource-class script-name)
  (:documentation "Creates and returns an object of type
RESOURCE-CLASS \(a subclass of RESOURCE) corresponding to the script
name SCRIPT-NAME \(which is already URL-decoded).")
  (:method ((resource-class standard-class) script-name)
   (create-resource (class-name resource-class) script-name))
  (:method ((resource-class-name symbol) script-name)
   (make-instance resource-class-name
                  :script-name script-name)))

(defun get-resource (&optional (script-name (url-decode* (script-name*))))
  "Creates and returns an object of the type stored in
*RESOURCE-CLASS* corresponding to the script name SCRIPT-NAME."
  (create-resource *resource-class* script-name))

(defun resource-created (resource)
  "Utility function which sets up Hunchentoot's *REPLY* object
for a +HTTP-CREATED+ response corresponding to the newly-created
resource RESOURCE."
  (setf (content-type*) (get-content-type resource)
        (header-out :location) (resource-script-name resource)
        (return-code*) +http-created+)
  (let ((etag (resource-etag resource))
        (content-language (resource-content-language resource)))
    (when etag
      (setf (header-out :etag) etag))
    (when content-language
      (setf (header-out :content-language) content-language)))
  nil)