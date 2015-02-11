(in-package :cl)

(defpackage :xmlw
  (:use #:cl)
  (:export #:namespace #:*xmlns* #:writing-xml
   #:with-tag #:tag
   #:attr
   #:content #:content-format
   #:cdata #:cdata-format))
