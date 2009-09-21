;; Copyright 2009, Georgia Tech Research Corporation
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

;; S-Expand DocBook Macros


(defpackage :s-expand-docbook
  (:use :cl)
  (:export :*docbook-transform-alist*))

(in-package :s-expand-docbook)

(defun transform-sect (level title &rest body)
  `(,(make-symbol (format nil "sect~A" level))
                  (title ,title)
                  ,@body))

(defun transform-docfun (name description result &rest args)
  `(para (programlisting 
          ,(format nil "~A ~{~A ~} =>  ~A" 
                   name (mapcar (lambda (arg)
                                  (if (atom arg) arg (car arg)))
                                  args)
                   result))
         ,@(if (atom description) (list description) description)
         ,@(let ((details (mapcan (lambda (arg) 
                                    (when (and (listp arg)
                                               (cdr arg))
                                      `((varlistentry (term (varname ,(car arg)))
                                                      (listitem (para
                                                                 ,@(cdr arg)))))))
                                  args)))
                (when details
                  `((variablelist ,@details))))))



(defparameter *docbook-transform-alist*
  `((:sect ,#'transform-sect)
    (:docfun ,#'transform-docfun)))
