;; Copyright 2009, Georgia Tech Research Corporation
;; Copyright 2015, Rice University
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;     * Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;
;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;;     * Neither the name of the Copyright Holders nor the names of
;;      its contributors may be used to endorse or promote products
;;      derived from this software without specific prior written
;;      permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;; Author: Neil T. Dantam
;; A quick hack to expand s-expressions into XML
;; Motivated by the inability to create macros in DocBook

(defpackage :s-expand
  (:use :cl)
  (:export :s-expand :s-expand-file))


(in-package :s-expand)

(defparameter *indent* 0)

(defparameter *indent-character* #\Space
  "Character to use for indentation")
(defparameter *indent-count* 2
  "Number of times to repeat *indent-character* at each indent level")

(defmacro with-indent (&body body)
  `(let ((*indent* (1+ *indent*)))
     ,@body))

(defun indent (stream)
  "Write indentation to stream."
  (let ((string (make-string (* *indent-count*
                                (max 0 *indent*))
                             :initial-element *indent-character*)))
    (write-string string stream)))


(defun s-expand-1 (atom)
  "Return the string expansions of an atom."
  (etypecase atom
    (symbol (string-downcase (symbol-name atom)))
    (string atom)))

(defun s-expand-tag (stream tag)
  "Expand a tag.

STREAM: output stream
TAG: An atom or cons tag"

  (let ((tag-name (s-expand-1 (if (atom tag) tag (car tag)))))
    (if (atom tag)
        (format stream "<~A>~%" tag-name)
        (format stream "<~A~{ ~A=\"~A\"~}>~%"
                tag-name
                (map 'list #'s-expand-1 (cdr tag))))
    tag-name))

(defun s-expand (stream sexpr &key transform-alist)
  (labels ((helper (sexpr)
             (etypecase sexpr
               (null)
               (cons
                (destructuring-bind (tag &rest body) sexpr
                  (if (and (atom tag)
                           (assoc tag transform-alist))
                    (helper (apply (cadr (assoc tag transform-alist)) body))
                    (progn
                      (indent stream)
                      (let ((tag-name (s-expand-tag stream tag)))
                        (with-indent (mapcan #'helper body))
                        (terpri)
                        (indent stream)
                        (format stream "</~A>" tag-name))))))
               (t (let ((str (s-expand-1 sexpr)))
                    (case (elt str 0)
                      ((#\, #\. #\;))
                      (otherwise (indent stream)))
                    (write-string str stream))))))
    (helper sexpr)))

(defun s-expand-file (filespec sexpr &key
                      (xml-version "1.0")
                      (xml-encoding "UTF-8")
                      (doctype-string "")
                      (if-exists :error)
                      transform-alist)
  (with-open-file (s filespec
                     :direction :output
                     :if-exists if-exists)
    (format s "<?xml version=\"~A\" encoding=\"~A\"?>~%"
            xml-version xml-encoding)
    (format s "~A~%" doctype-string)
    (s-expand s sexpr :transform-alist transform-alist)))
