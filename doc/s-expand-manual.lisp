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
         ,description
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
      (para "This is just a quick hack to convert s-expressions to
             XML.  It came about after an attempt to write DocBook and
             great frustration over the (apparent) lack of
             TeX-comprable macros as well as XML's extreme
             verbosity."))
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
         (listitem (docfun s-expand "Expands an sexpression into XML"
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
                          the list."))))))))



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
