;; Copyright 2009, Georgia Tech Research Corporation
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;;     * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;;     * Neither the name of Google Inc. nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Author: Neil T. Dantam
;; A quick hack to expand s-expressions into XML
;; Motivated by the inability to create macros in DocBook

(defun s-expand-1 (atom)
  (etypecase atom
    (symbol (string-downcase (symbol-name atom)))
    (string atom)))
    
(defun s-expand-tag (stream tag)
  (labels ((print-attributes (list)
             (when list
               (format stream " ~A=\"~A\"" 
                       (s-expand-1 (car list)) (s-expand-1 (cadr list)))
               (print-attributes (cddr list)))))
    (let ((tag-name (if (atom tag) 
                        (s-expand-1 tag)
                        (s-expand-1 (car tag)))))
      (if (atom tag)
          (format stream "<~A>" tag-name)
          (progn 
            (format stream "<~A" tag-name)
            (print-attributes (cdr tag))
            (format stream ">")))
      tag-name)))

(defun s-expand (stream sexpr)
  (cond 
    ((null sexpr))
    ((listp sexpr)
     (let ((tag-name (s-expand-tag stream (car sexpr))))
       (mapcan (lambda (sexpr)
                 (s-expand stream sexpr))
               (cdr sexpr))
       (format stream "</~A>" tag-name)))
    (t (write-string (s-expand-1 sexpr) stream))))


(defun s-expand-file (filespec sexpr &key
                      (xml-version "1.0")
                      (xml-encoding "UTF-8")
                      (doctype-string "")
                      (if-exists :error))
  (with-open-file (s filespec 
                     :direction :output
                     :if-exists if-exists)
    (format s "<?xml version=\"~A\" encoding=\"~A\"?>~%"
            xml-version xml-encoding)
    (format s "~A~%" doctype-string)
    (s-expand s sexpr)))
