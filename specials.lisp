;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-WEBDAV; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-webdav/specials.lisp,v 1.10 2008/06/25 08:02:17 edi Exp $

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

#+:sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defconstant (name value &optional doc)
    "Make sure VALUE is evaluated only once \(to appease SBCL)."
    `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
       ,@(when doc (list doc)))))

(defun constantly-nil (&rest args)
  "Does the same as the result of \(CONSTANTLY NIL)."
  (declare (ignore args))
  nil)

(defconstant +dav-property-alist+
  '(("creationdate" . creation-date)
    ("displayname" . resource-display-name)
    ("getcontentlength" . get-content-length)
    ("getcontenttype" . get-content-type)
    ("getetag" . resource-etag)
    ("getlastmodified" . get-last-modified)
    ("getcontentlanguage" . resource-content-language)
    ("resourcetype" . resource-type)
    ("source" . resource-source)
    ("lockdiscovery" . constantly-nil)
    ("supportedlock" . constantly-nil))
  "An alist mapping the \(names of the) standard DAV properties
to functions handling them.")

(defvar *dav-compliance-classes* '(1)
  "A /sorted/ list of DAV compliance classes reported in the
`DAV' header when answering OPTIONS requests.  It doesn't make
much sense to have more then class 1 in here as long as there's
no lock support.")

(defvar *allowed-methods*
  '(:options :get :head :delete :propfind :proppatch :put :copy :move :mkcol)
  "The list of methods \(as keywords) returned by the `Allow'
header in case of OPTIONS requests \(and also utilized by the
handler for MKCOL).  Can be adapted to allow for more methods,
but for a WebDAV server at least the methods above should be
listed.")

(defvar *resource-class* 'file-resource
  "Whenever a DAV handler is executed, this variable should be bound
to the resource class which is to be used.  If you're using
CREATE-DAV-DISPATCHER, this will already be taken care of for you.")

(defvar *file-resource-base-uri* ""
  "The value of this variable is the return value of the default
method for FILE-RESOURCE-BASE-URI.  It should be a string which starts
with a slash if it's not empty and does /not/ end with a slash and is
/not/ URL-encoded.")

(defvar *file-resource-base-path-namestring*
  (namestring (truename (ensure-directories-exist "/tmp/")))
  "The value of this variable is the return value of the default
method for FILE-RESOURCE-BASE-PATH-NAMESTRING.  It should be the
namestring of the truename of an absolute pathname denoting a
directory, specifically it must return a string starting and
ending with slashes.  \(Note: This should work on Windows as
well.)")

(defconstant +buffer-length+ 8192
  "Length of buffers used for internal purposes.")

(defconstant +latin-1+
  (flex:make-external-format :latin-1 :eol-style :lf)
  "A FLEXI-STREAMS external format for ISO-8859-1.")

(defconstant +utf-8+
  (flex:make-external-format :utf-8 :eol-style :lf)
  "A FLEXI-STREAMS external format for UTF-8.")

(defvar *property-hash* (make-hash-table :test #'equal)
  "The hash table that by default is used to store dead
properties in RAM.")

(defconstant +webdav-dtd+
  ;; we can't use CXML's DTD support directly because a) DTDs don't
  ;; work with namespaces and b) the DTD in RFC 2518 is broken anyway
  '(("activelock" "lockscope" "locktype" "depth" (? "owner") (? "timeout") (? "locktoken"))
    ("allprop" :empty)
    ("collection" :empty)
    ("creationdate" :pcdata)
    ("depth" :pcdata)
    ("displayname" :pcdata)
    ("dst" :pcdata)
    ("exclusive" :empty)
    ("getcontentlanguage" :pcdata)
    ("getcontentlength" :pcdata)
    ("getcontenttype" :pcdata)
    ("getetag" :pcdata)
    ("getlastmodified" :pcdata)
    ("href" :pcdata)
    ;; the following is not allowed in DTDs
    ("keepalive" (:choice :pcdata (+ "href")))
    ("link" (+ "src") (+ "dst"))
    ("lockdiscovery" (* "activelock"))
    ("lockentry" "lockscope" "locktype")
    ("lockinfo" "lockscope" "locktype" (? "owner"))
    ("lockscope" (:choice "exclusive" "shared"))
    ("locktoken" (+ "href"))
    ("locktype" "write")
    ("multistatus" (+ "response") (? "responsedescription"))
    ("omit" :empty)
    ("owner" :any)
    ("prop" :any)
    ("propertybehavior" (:choice "omit" "keepalive"))
    ("propertyupdate" (+ (:choice "remove" "set")))
    ("propfind" (:choice "allprop" "propname" "prop"))
    ("propname" :empty)
    ("propstat" "prop" "status" (? "responsedescription"))
    ("remove" "prop")
    ("resourcetype" :any)
    ("response" "href" (:choice ((* "href") "status") (+ "propstat")) (? "responsedescription"))
    ("responsedescription" :pcdata)
    ("set" "prop")
    ("shared" :empty)
    ("source" (* "link"))
    ("src" :pcdata)
    ("status" :pcdata)
    ("supportedlock" (* "lockentry"))
    ("timeout" :pcdata)
    ("write" :empty))
  "This is used to validate incoming XML.  Obviously, this is not
a \"real\" DTD, but the idea should be clear.")