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

(require :s-expand)

(use-package :s-expand)

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


(defparameter *doc*
  `(article
    (title "S-EXPAND")
    (sect 1 "Introduction"
      (sect 2 "Overview"
        (para "This is just a quick hack to convert S-Expressions to
             XML.  It came about after an attempt to write DocBook and
             great frustration over the (apparent) lack of
             TeX-comprable macros as well as XML's extreme
             verbosity."))
      (sect 2 "Getting S-EXPAND"
        (programlisting 
         ,(concatenate 'string
                      "git clone "
                      "http://www.prism.gatech.edu/"
                      "~ndantam3/git/s-expand.git")))
      (sect 2 "Legal"
        (para
         "Copyright" ((trademark class copyright)) "2009 Georigia Tech
         Research Corporation." 
         "All rights reserved.")
        (para
         "Redistribution and use in source and binary forms, with or
          without modification, are permitted provided that the
          following conditions are met:")
        (itemizedlist
         (listitem (para "Redistributions of source code must retain the
          above copyright notice, this list of conditions and the
          following disclaimer."))
         (listitem (para "Redistributions in binary form must reproduce the
          above copyright notice, this list of conditions and the
          following disclaimer in the documentation and/or other
          materials provided with the distribution."))
         (listitem (para "Neither the name of the Copyright Holders
          nor the names of their contributors may be used to endorse
          or promote products derived from this software without
          specific prior written permission.")))

         (para "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
          CONTRIBUTORS \"AS IS\" AND ANY EXPRESS OR IMPLIED
          WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
          WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
          PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
          OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
          INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
          (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
          GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
          BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
          LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
          TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
          OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
          POSSIBILITY OF SUCH DAMAGE.")))
    (sect 1 "Usage"
      (sect 2 "Overview"
           (para "Each XML element is represented as a nested S-Expression.
             The CAR of the sexpr is the tag name of the element.  If
             the tag needs attributes, then the tag should be given as
             a list with the CAR of that list being the tag name and
             the CDR being a property list of attribte-names and
             attribute-values."))
      (sect 2 "Representation Example"
        (example 
         (title "Representation")
         (programlisting 
          ,(concatenate 'string
                        "(strong \"Text\")"
                        "  =>  "
                        "&lt;strong&gt;Text&lt;/strong&gt;"))
       (programlisting 
        ,(concatenate 'string
                      "((a href \"foo.txt\") Link)"
                      "  =>  "
                      "&lt;a href=\"foo.txt\"&gt;Link&lt;/a&gt;"))
       (programlisting 
        ,(concatenate 'string
                      "(p symbol \"text\" (strong words))"
                      "  =>  "
                      "&lt;p&gt;symbol text "
                      "&lt;strong&gt;words&lt;/strong&gt;&lt;p&gt;"))))
      (sect 2 "Transform Functions"
        (para "This is the cool part.  S-EXPAND allows you define
        functions that it will call on patterns in the input sexpr.
        You give it a symbol, and a function.  Then, for all instances
        where that symbol is the car of a list, it will call the
        function of the cdr of that list and recursively expand the
        result.  We now have XML macros."))
      (sect 2 "Exported Functions"
        (para "You'll probably want to use S-EXPAND by creating
           templates using the " (function "BACKQUOTE") " operator and
           passing those to one of either " (function "s-expand") " or
           " (function "s-expand-file") ".")
        ;; now list the functions
        (itemizedlist
         (listitem (docfun s-expand "Expands an sexpr into XML"
                           nil 
                           (stream "The stream to output XML on.  Could
                         be " (literal t) "to print to standard
                         output.")
                           (sexpr "The sexpression that's being translated")
                           "&amp;key"
                           (transform-alist 
                            "An association list of elements whose
                          cadr is a transform function (described
                          above).  The keys in the assoc list are
                          symbols to match against the input.  The
                          cadrs of list elements are the transform
                          functions whose results get expanded into
                          the list.")))
         (listitem (docfun s-expand-file
                           ("Expands sexpr into an XML file given by"
                            (varname filespec))
                           nil
                           (filespec "Pathname designator")
                           (sexpr "The S-Expression to translate")
                           "&amp;key"
                           (xml-version "XML version number, defaults to \"1.0\"")
                           (xml-encoding "XML encoding type, defaults to \"UTF-8\"")
                           (doctype-string "The XML DOCTYPE")
                           (if-exists "Passed on to " (function with-open-file))
                           (transform-alist "Passed on to " (function s-expand)))))))))



(defparameter *transform-alist*
  `((sect ,#'transform-sect)
    (docfun ,#'transform-docfun)))

(defun expand-inline ()
  (s-expand t *doc* :transform-alist *transform-alist*))

(defun expand ()
  (s-expand-file "s-expand-manual.xml" *doc*
                 :doctype-string
                 (concatenate 'string
                              "<!DOCTYPE article "
                              "PUBLIC \"-//OASIS//DTD DocBook XML V4.4//EN\" "
                              "\"http://docbook.org/xml/4.2/docbookx.dtd\">")
                 :if-exists :supersede
                 :transform-alist *transform-alist*))
