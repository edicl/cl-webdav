;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-WEBDAV; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-webdav/xml.lisp,v 1.10 2007/10/21 21:20:56 edi Exp $

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

(defclass xmls-builder (sax:default-handler)
  ((element-stack :initform nil
                  :accessor element-stack
                  :documentation "Used to keep track of the stack
of \"open\" \(encountered SAX:START-ELEMENT, but didn't see
SAX:END-ELEMENT yet) elements.")
   (root :initform nil
         :accessor root
         :documentation "The root of the XML document."))
  (:documentation "This is like CXML's XMLS-BUILDER class, but
without attempting to be compatible with XMLS, so we can get
namespaces right."))

(defun make-xmls-builder ()
  "Creates and returns an instance of the XMLS-BUILDER class."
  (make-instance 'xmls-builder))

(defun make-xmls-node (&key local-name namespace-uri attributes children)
  "Creates and returns an XMLS node with the local name
LOCAL-NAME, the namespace URI NAMESPACE-URI, the attributes
ATTRIBUTES \(a list of attributes) and the children CHILDREN
\(also a list).  An XMLS node is a list with two or more elements
where the first element is the name, the second element is the
list of attributes, and all the following elements \(if any) are
the children.  The name is either a string \(if there is no
namespace), or a cons where the car is the local name and the cdr
is the namespace URI.

An attribute is a list of two elements where the first element is
the name and the second element is the value.  Attribute names
are structured like XMLS node names."
  (list* (cond (namespace-uri (cons local-name namespace-uri))
               (t local-name))
         attributes
         children))

(defun dav-node (local-name &rest children)
  "Shortcut for MAKE-XMLS-NODE.  Returns an XMLS node with the
local name LOCAL-NAME, the namespace URI \"DAV:\", and the
children CHILDREN."
  (make-xmls-node :local-name local-name
                  :namespace-uri "DAV:"
                  :children children))

(defun namespace-uri (thing)
  "Returns the namespace URI \(which can be NIL) of the XMLS node
or attribute THING."
  (etypecase (car thing)
    (string nil)
    (cons (cdar thing))))

(defun local-name (thing)
  "Returns the local name of the XMLS node or attribute THING."
  (etypecase (car thing)
    (string nil)
    (cons (caar thing))))

(defun node-attributes (xmls-node)
  "Returns the list of attributes of the XMLS node XMLS-NODE."
  (second xmls-node))

(defun (setf node-attributes) (attributes xmls-node)
  "Sets the list of attributes of the XMLS node XMLS-NODE to
ATTRIBUTES."
  (setf (second xmls-node) attributes))

(defun node-children (xmls-node)
  "Returns the list of children of the XMLS node XMLS-NODE."
  (cddr xmls-node))

(defun (setf node-children) (children xmls-node)
  "Sets the list of children of the XMLS node XMLS-NODE to
CHILDREN."
  (setf (cddr xmls-node) children))

(defmethod sax:end-document ((handler xmls-builder))
  "The last method to be called when parsing an XML document -
returns the root of the document."
  (root handler))

(defmethod sax:start-element ((handler xmls-builder) namespace-uri local-name qname attributes)
  "The method to be called when the SAX parser encounters the
start of an element."
  ;; the local name is in QNAME if a namespace wasn't specified
  (setf local-name (or local-name qname))
  (let* ((attributes
          (loop for attribute in attributes
                for attribute-namespace-uri = (sax:attribute-namespace-uri attribute)
                when (and (sax:attribute-specified-p attribute)
                          ;; ignore standard "xmlns:" namespace 
                          (not (equal attribute-namespace-uri "http://www.w3.org/2000/xmlns/")))
                collect (list (cond (attribute-namespace-uri
                                     (cons (sax:attribute-local-name attribute)
                                           attribute-namespace-uri))
                                    (t (sax:attribute-qname attribute)))
                              (sax:attribute-value attribute))))
         (xmls-node (make-xmls-node :local-name local-name
                                    :namespace-uri namespace-uri
                                    :attributes attributes))
         (parent (car (element-stack handler))))
    ;; add this element to the children of the "current parent" if
    ;; there is one - otherwise this is the root of the XMLS document
    (cond (parent (push xmls-node (node-children parent)))
          (t (setf (root handler) xmls-node)))
    ;; make this one the "current parent"
    (push xmls-node (element-stack handler))))

(defmethod sax:end-element ((handler xmls-builder) namespace-uri local-name qname)
  "The method to be called when the SAX parser encounters the end
of an element."
  (declare (ignore namespace-uri local-name qname))
  ;; remove element from stack of "current ancestors" and correct
  ;; order of children
  (let ((node (pop (element-stack handler))))
    (setf (node-children node) (nreverse (node-children node)))))

(defmethod sax:characters ((handler xmls-builder) data)
  "The method to be called when the SAX parser encounters
character content."
  (let* ((parent (car (element-stack handler)))
         (prev (car (node-children parent))))
    ;; add characters to previous child if that one is character data,
    ;; otherwise start a new child
    (cond ((typep prev '(or rod string))
           (setf (car (node-children parent))
                 (concatenate `(vector ,(array-element-type prev)) prev data)))
          (t (push data (node-children parent))))))

(defun walk-xmls-node (handler xmls-node)
  "Helper function for SERIALIZE-XMLS-NODE.  Walks recursively
through the XMLS node XMLS-NODE and calls the SAX handler HANDLER
when appropriate."
  (let ((counter -1))
    (labels ((next-prefix ()
               "Returns a short string which can be used as a
namespace prefix and which hasn't been used so far in the same
node."
               (format nil "~V,,,VA"
                       (1+ (floor (incf counter) 26))
                       (code-char (+ (mod counter 26) #.(char-code #\A)))
                       ""))
             (maybe-make-rod (thing)
               "Returns the corresponding \"rod\" \(see CXML
documentation) for THING.  Returns NIL if THING is NIL."
               (and thing (rod thing)))
             (make-attribute (&key namespace-uri local-name qname value)
               "Returns a SAX attribute corresponding to the
keyword arguments."
               (sax:make-attribute :namespace-uri (maybe-make-rod namespace-uri)
                                   :local-name (maybe-make-rod local-name)
                                   :qname (maybe-make-rod qname)
                                   :value (maybe-make-rod value)))
             (make-xmlns-attribute (prefix namespace-uri)
               "Returns an XML namespace attribute for the
namespace URI NAMESPACE-URI and the prefix PREFIX."
               (make-attribute :namespace-uri "http://www.w3.org/2000/xmlns/"
                               :local-name prefix
                               :qname (format nil "xmlns:~A" prefix)
                               :value namespace-uri))
             (compute-attributes (attributes)
               "Converts the list ATTRIBUTES of XMLS attribute to
a list of SAX attributes.  Takes care of attribute namespaces as
well."
               (loop for attribute in attributes
                     for (name value) = attribute
                     for namespace-uri = (namespace-uri attribute)
                     for local-name = (local-name attribute)
                     for prefix = (and namespace-uri (next-prefix))
                     when namespace-uri                   
                     collect (make-attribute :namespace-uri namespace-uri
                                             :local-name local-name
                                             :qname (format nil "~A:~A" prefix local-name)
                                             :value value)
                     and
                     collect (make-xmlns-attribute prefix namespace-uri)
                     else
                     collect (make-attribute :local-name name
                                             :qname name
                                             :value value))))
      (let* ((attribute-list (compute-attributes (node-attributes xmls-node)))
             (namespace-uri (namespace-uri xmls-node))
             (local-name (local-name xmls-node))
             (qname local-name))
        (when namespace-uri
          (let ((prefix (next-prefix)))
            (push (make-xmlns-attribute prefix namespace-uri)
                  attribute-list)
            (setq qname (format nil "~A:~A" prefix local-name)
                  namespace-uri (rod namespace-uri))))
        (setq local-name (rod local-name)
              qname (rod qname))
        (sax:start-element handler namespace-uri local-name qname attribute-list)
        ;; apply WALK-XMLS-NODE the the children recursively
        (dolist (child (node-children xmls-node))
          (typecase child
            (list (walk-xmls-node handler child))
            ((or string rod) (sax:characters handler (rod child)))))
        (sax:end-element handler namespace-uri local-name qname)))))

(defun serialize-xmls-node (xmls-node)
  "Serializes XMLS-NODE to a vector of octets which is returned.

This is kind of the inverse operation to PARSE-DAV and very
similar to CXML-XMLS:MAP-NODE."
  (let ((handler (make-octet-vector-sink)))
    (sax:start-document handler)
    (walk-xmls-node handler xmls-node)
    (sax:end-document handler)))

(defun xmls-node-p (thing)
  "Checks whether THING is an XMLS node.  See the docstring of
MAKE-XMLS-NODE for a description of the structure of XMLS nodes."
  (or (null thing)
      (and (listp thing)
           ;; the name
           (let ((tag (first thing)))
             (or (stringp tag)
                 (and (consp tag)
                      (stringp (car tag))
                      (stringp (cdr tag)))))
           ;; the attributes
           (loop for attribute in (node-attributes thing)
                 always (and (listp attribute)
                             (null (cddr attribute))
                             ;; the attribute's name
                             (let ((name (first attribute)))
                               (or (stringp name)
                                   (and (consp name)
                                        (stringp (car name))
                                        (stringp (cdr name)))))
                             ;; the attribute's value
                             (stringp (second attribute))))
           ;; the children
           (loop for child in (node-children thing)
                 always (or (stringp child) (xmls-node-p child))))))

(defun remove-content (xmls-node)
  "Returns an XMLS node which has the same name and attributes as
the XMLS node XMLS-NODE but an empty body."
  (subseq xmls-node 0 2))

(defun spec (node)
  "Returns the corresponding \"spec\" from +WEBDAV-DTD+ if NODE
is a DAV node in XMLS format."
  (and (xmls-node-p node)
       (equal (namespace-uri node) "DAV:")
       (cdr (assoc (local-name node) +webdav-dtd+ :test #'string=))))

(defgeneric validate (node spec pos)
  (:documentation "Validates the node NODE against the spec \(see
function SPEC) SPEC.  SPEC can be a string \(denoting the local
name of a DAV node), one of the symbols :ANY, :EMPTY, or :PCDATA,
a two element list beginning with * or ?, a list beginning
with :CHOICE, or any other list denoting a sequence.  If POS is
NIL, SPEC must be a string and the node itself is validated to
conform to SPEC and a true value is returned iff that's the case.
Otherwise, POS should be a non-negative integer N, and the Nth
child of NODE will be validated.  The true value returned in this
case \(i.e. if the Nth child is valid) will be another integer
denoting the next child which has to be validated.  Furthermore,
whitespace is removed from elements which can only have element
content."))

(defmethod validate (node (spec string) pos)
  "Validates NODE or the child at position POS to conform to the
spec for the DAV node named SPEC."
  (unless (xmls-node-p node)
    ;; not valid if this is character content
    (return-from validate nil))
  (let ((children (node-children node)))
    (cond (pos
           ;; function call itself recursively with POS set to NIL and
           ;; the child at position POS as the first element
           (and (< pos (length children))
                (validate (nth pos children) spec nil)
                ;; increment POS if successful
                (1+ pos)))
          ((and (equal (local-name node) spec)
                (let ((spec (spec node)))
                  (unless (member spec '((:pcdata) (:any)) :test #'equal)
                    ;; remove whitespace if we only expect element content
                    (setf (node-children node)
                            (remove-if #'whitespace-string-p children)))
                  (and spec
                       (let ((end-pos (validate node spec 0)))
                         (and end-pos
                              (or (>= end-pos (length (node-children node)))))))))))))

(defmethod validate (node (spec (eql :any)) pos)
  "Always succeeds and eats up all remaining children of NODE."
  (length (node-children node)))

(defmethod validate (node (spec (eql :empty)) pos)
  "Succeeds \(with a return value of 1) iff there are no
children."
  ;; note that this assumes that POS is 0 - see +WEBDAV-DTD+
  (and (null (node-children node))
       1))

(defmethod validate (node (spec (eql :pcdata)) pos)
  "Succeeds \(and this increments POS by 1) if the child at
position POS is not an XMLS node \(i.e. if it's character data)."
  (let ((children (node-children node)))
    (and (< pos (length children))
         (not (xmls-node-p (nth pos children)))
         (1+ pos))))

(defmethod validate (node (spec list) pos)
  "Compound validation for choices, repetitions, and sequences -
dispatces on the first element of SPEC."
  (case (first spec)
    (:choice
     ;; succeeds if one of the choices succeeds
     (loop for spec-part in (rest spec)
           thereis (validate node spec-part pos)))
    (?
     ;; always succeeds, but increments POS if the inner spec succeeds
     (if (validate node (second spec) pos) (1+ pos) pos))
    ((+ *)
     ;; * will always succeed, + will succeed if there's at least one
     ;; match - both try to validate repeatedly and increment POS by
     ;; the number of matches
     (let ((inner-spec (second spec))
           (optionalp (eq spec '*)))
       (and (or optionalp (validate node inner-spec pos))
            (loop for next-pos from (if optionalp pos (1+ pos))
                  unless (validate node inner-spec next-pos)
                  do (return next-pos)))))
    (otherwise
     ;; this is a sequence, i.e. we only suceed if each spec succeeds
     ;; in turn; we increment POS as we walk through the sequence
     (loop for inner-spec in spec
           for next-pos = (validate node inner-spec pos)
           then (validate node inner-spec next-pos)
           always next-pos
           finally (return next-pos)))))

(defun ignore-non-dav-elements (node)
  "Helper function which recursively walks the XMLS node NODE and
returns an equivalent node with all non-DAV elements removed
\(except for those in :ANY positions)."
  (cond ((or (atom node)
             (equal (spec node) '(:any))) node)
        ((not (equal (namespace-uri node) "DAV:")) nil)
        (t (let ((children (loop for child in (node-children node)
                                 for converted-child = (ignore-non-dav-elements child)
                                 when converted-child
                                 collect it)))
             (make-xmls-node :local-name (local-name node)
                             :namespace-uri (namespace-uri node)
                             :attributes (node-attributes node)
                             :children children)))))

(defun parse-dav (octets &optional root-name)
  "Accepts an array OCTETS of octets representing a DAV XML node and
converts it into the corresponding XMLS node.  According to the WebDAV
RFC, non-DAV elements are skipped unless they appear in positions
\(like in a \"prop\" element) where arbitrary elements are allowed.
If ROOT-NAME is given, it should be the local name \(a string) of a
DAV node.  In this case, the XML is validated.  This function is
expected to be called from within a Hunchentoot request and throws to
the HANDLER-DONE tag with a return code of +HTTP-BAD-REQUEST+ if a
parsing error occurs or if the XML is invalid.

This is kind of the inverse operation to SERIALIZE-XMLS-NODE."
  (let ((node (handler-case
                  (ignore-non-dav-elements
                   (parse-octets octets (make-xmls-builder)))
                (xml-parse-error (c)
                  (warn "XML parse error: ~A" c)
                  (bad-request)))))
    (cond ((or (null root-name)
               (validate node root-name nil)) node)
          (t (warn "Couldn't validate ~S." node)
             (bad-request)))))