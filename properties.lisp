;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-WEBDAV; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-webdav/properties.lisp,v 1.9 2008/06/25 08:04:25 edi Exp $

;;; Copyright (c) 2007-2010, Dr. Edmund Weitz.  All rights reserved.

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

(defun property-equal (property1 property2)
  "Two \(XMLS nodes denoting) properties are equal iff they have
the same local name and the same namespace URI."
  (and (equal (namespace-uri property1)
              (namespace-uri property2))
       (equal (local-name property1)
              (local-name property2))))

(defun dav-property-function (property)
  "Returns the function which is responsible to retrieve the DAV
\"live\" property PROPERTY, where PROPERTY can either be an XMLS
node or a string denoting the local name of the corresponding
node.  If PROPERTY doesn't denote a DAV property, this function
returns NIL."
  (etypecase property
    (string (cdr (assoc property +dav-property-alist+ :test #'string=)))
    ((satisfies xmls-node-p)
     (and (equal (namespace-uri property) "DAV:")
          (dav-property-function (local-name property))))))

(defun get-dead-property (resource property-designator)
  "Returns the dead property denoted by the XMLS node
PROPERTY-DESIGNATOR of the resource RESOURCE or +HTTP-NOT-FOUND+
if such a property doesn't exist."
  (or (find property-designator (get-dead-properties resource)
            :test #'property-equal)
      +http-not-found+))

(defun get-property (resource property-designator)
  "Returns the \(live or dead) property denoted by the XMLS node
PROPERTY-DESIGNATOR of the resource RESOURCE or +HTTP-NOT-FOUND+
if such a property doesn't exist."
  (let ((property-function (dav-property-function property-designator)))
    (cond (property-function
           ;; if there's such a function, this is a live property
           (let ((property (funcall property-function resource)))
             (cond ((stringp property)
                    (dav-node (local-name property-designator) property))
                   ((xmls-node-p property) property))))
          (t (get-dead-property resource property-designator)))))

(defun propstat (resource property-designator show-content)
  "Uses GET-PROPERTY to try to retrieve the property designated
by PROPERTY-DESIGNATOR from the resource RESOURCE.  Returns as
its first value an HTTP return code \(with +HTTP-OK+ denoting
that the property was found) and as its second value the property
designator or \(if SHOW-CONTENT is true and the property was
found) the property itself."
  (let ((property (handler-case
                      (get-property resource property-designator)
                    (error (condition)
                      (warn
                       "While trying to get property ~S for resource ~S: ~A"
                       (local-name property-designator)
                       (resource-script-name resource)
                       condition)
                      +http-internal-server-error+))))
    (etypecase property
      (null (values +http-ok+ property-designator))
      (integer (values property property-designator))
      ((satisfies xmls-node-p)
       (values +http-ok+
               (cond (show-content property)
                     (t property-designator)))))))

(defun all-dead-property-designators (resource)
  "Returns a list of XMLS nodes which are designators for all
dead properties for the resource RESOURCE."
  (loop for property in (get-dead-properties resource)
        collect (remove-content property)))

(defconstant +dav-property-designators+
  (load-time-value
   (loop for (name . nil) in +dav-property-alist+
         collect (dav-node name)))
  "A list of XMLS nodes which are property designators for all
DAV \(live) properties defined by the WebDAV RFC.  Computed at
load time from +DAV-PROPERTY-ALIST+.")

(defun all-property-designators (resource)
  "Returns a list of XMLS nodes which are designators for all
\(dead and live) properties for the resource RESOURCE."
  (append +dav-property-designators+
          (all-dead-property-designators resource)))

(defun collect-properties (resource property-designators show-content)
  "Tries to collect all properties of the resource RESOURCE which
are in the list PROPERTY-DESIGNATORS \(which can also be the
symbol T which means \"all dead an live properties\".
SHOW-CONTENT is interpreted as in PROPSTAT.  Returns the
corresponding \"response\" DAV node as an XMLS node."
  ;; we use a hash table to map status codes to properties for the XML
  ;; response
  (let ((status-hash (make-hash-table)))
    (dolist (property-designator (cond ((eql property-designators t)
                                        (all-property-designators resource))
                                       (t property-designators)))
      (multiple-value-bind (status property)
          (propstat resource property-designator show-content)
        (push property (gethash status status-hash))))
    (let ((propstats
           (loop for status being the hash-keys of status-hash
                 using (hash-value properties)
                 ;; collect all properties with the same status code
                 ;; within one "propstat" element
                 collect (dav-node "propstat"
                                   (apply #'dav-node "prop" properties)
                                   (dav-node "status" (status-line status))))))
      ;; create the XMLS "response" node
      (apply #'dav-node "response"
             (dav-node "href" (resource-href resource))
             propstats))))

(defun parse-propfind (octets)
  "Helper function for PROPFIND-HANDLER which parses the XML
stored in OCTETS \(an array of octets) which is supposed to be
either empty or the body of a PROPFIND request.  Returns as its
first value a list of the requested properties \(as XMLS nodes)
or T \(denoting all properties) and as its second value a boolean
which is T iff the client wants only property names."
  (when (zerop (length octets))
    (return-from parse-propfind t))
  (let* ((node (first (node-children (parse-dav octets "propfind"))))
         (node-name (local-name node)))
    (cond ((string= node-name "allprop") t)
          ((string= node-name "propname") (values t t))
          ((string= node-name "prop")
           ;; skip whitespace (which hasn't been removed as the
           ;; "spec" is :ANY)
           (remove-if #'whitespace-string-p (node-children node))))))

;;; The functions below implement the very simple protocol of storing
;;; dead properties in a hash table in RAM, i.e. without persistence -
;;; don't feel tempted to use this for a production system

(defun retrieve-properties (key)
  "Retrieves the properties stored under the \(EQUAL) key KEY."
  (gethash key *property-hash*))

(defun store-properties (key properties)
  "Stores PROPERTIES under the \(EQUAL) key KEY."
  (setf (gethash key *property-hash*) properties))

(defun remove-properties (key)
  "Removes any properties stored under the \(EQUAL) key KEY."
  (remhash key *property-hash*))

(defun move-properties (from-key to-key)
  "Moves properties stored under the \(EQUAL) key FROM-KEY \(if
any) to the key TO-KEY."
  (setf (gethash to-key *property-hash*)
        (gethash from-key *property-hash*))
  (remhash from-key *property-hash*))

(defun copy-properties (from-key to-key)
  "Copies \(using COPY-TREE) properties stored under the \(EQUAL)
key FROM-KEY \(if any) to the key TO-KEY."
  (setf (gethash to-key *property-hash*)
        ;; actually, due to how this is implemented, COPY-TREE isn't
        ;; really necessary, but it's better to be safe than sorry...
        (copy-tree (gethash from-key *property-hash*))))
