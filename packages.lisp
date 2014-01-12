;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-webdav/packages.lisp,v 1.16 2007/04/18 19:49:32 edi Exp $

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

(in-package :cl-user)

(defpackage :cl-webdav
  (:nicknames :dav)
  (:use :cl :hunchentoot :cxml)
  (:shadow #+:sbcl :defconstant)
  (:import-from :runes :rod)
  (:export :*allowed-methods*
           :*dav-compliance-classes*
           :*file-resource-base-path-namestring*
           :*file-resource-base-uri*
           :*resource-class*
           :accept-request-p
           :authorized-file-resource
           :copy-dead-properties
           :copy-resource
           :create-collection
           :create-dav-dispatcher
           :dav-node
           :file-resource
           :file-resource-base-path-namestring
           :file-resource-base-uri
           :get-content
           :get-dead-properties
           :local-name
           :move-dead-properties
           :move-resource
           :namespace-uri
           :node-attributes
           :node-children
           :options-handler
           :options-dispatcher
           :parse-dav
           :remove-dead-properties
           :remove-dead-property
           :remove-resource
           :resource
           :resource-children
           :resource-collection-p
           :resource-content-language
           :resource-content-type
           :resource-creation-date
           :resource-display-name
           :resource-etag
           :resource-exists
           :resource-length
           :resource-parent
           :resource-script-name
           :resource-source
           :resource-type
           :resource-uri-prefix
           :resource-write-date
           :send-content
           :serialize-xmls-node
           :set-dead-property
           :xmls-node-p))
