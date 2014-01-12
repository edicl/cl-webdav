;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-WEBDAV; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-webdav/util.lisp,v 1.3 2007/04/17 07:42:08 edi Exp $

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

(defmacro define-return-code-shortcut (name return-code)
  "Defines a function called NAME which just sets the HTTP return code
to RETURN-CODE and then ends the current handler by calling
ABORT-REQUEST-HANDLER."
  `(defun ,name ()
     ,(format nil "Sets RETURN-CODE to ~A and then calls ABORT-REQUEST-HANDLER."
              return-code)
     (setf (return-code*) ,return-code)
     (abort-request-handler)))

(define-return-code-shortcut not-implemented +http-not-implemented+)
(define-return-code-shortcut bad-request +http-bad-request+)
(define-return-code-shortcut bad-gateway +http-bad-gateway+)
(define-return-code-shortcut not-found +http-not-found+)
(define-return-code-shortcut forbidden +http-forbidden+)
(define-return-code-shortcut conflict +http-conflict+)
(define-return-code-shortcut precondition-failed +http-precondition-failed+)
(define-return-code-shortcut failed-dependency +http-failed-dependency+)
(define-return-code-shortcut method-not-allowed +http-method-not-allowed+)

(defun status-line (return-code)
  "Returns a full HTTP/1.1 status line corresponding to the
return code RETURN-CODE."
  (format nil "HTTP/1.1 ~D ~A" return-code
          (or (reason-phrase return-code)
              (error "Hunchentoot doesn't know the return code ~S."
                     return-code))))

(defun starts-with-p (seq prefix &key (test #'char=))
  "Returns a true value if the sequence SEQ starts with the
sequence PREFIX whereby the elements are compared using TEST."
  (let ((mismatch (mismatch seq prefix :test test)))
    (or (null mismatch)
        (= mismatch (length prefix)))))

(defun iso-8601-date (universal-time)
  "Returns a string representing the universal time
UNIVERSAL-TIME as an ISO-8601-formatted date."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(defun whitespace-string-p (thing)
  "Returns a true value if THING is a string consisting solely of
whitespace."
  (and (stringp thing) (ppcre:scan "^\\s*$" thing)))

(defun md5-hex (string)
  "Calculates the md5 sum of the string STRING and returns it as a hex string."
  (with-output-to-string (s)
    (loop for code across (md5:md5sum-sequence string)
	  do (format s "~2,'0x" code))))

(defun url-decode* (string)
  "Tries to URL-decode STRING with the UTF-8 external format first and
then uses LATIN-1 if that fails."
  ;; this is necessary because some clients use UTF-8 and some use
  ;; LATIN-1...
  (handler-case
      (url-decode string +utf-8+)
    (flex:external-format-encoding-error ()
      (url-decode string +latin-1+))))
