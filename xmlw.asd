(in-package #:cl)
(defpackage #:xmlw-system (:use #:asdf #:cl))
(in-package #:xmlw-system)

(asdf:defsystem xmlw
  :name "xmlw"
  :author "Jussi Lahdenniemi <jussi@lahdenniemi.fi>"
  :maintainer "Jussi Lahdenniemi <jussi@lahdenniemi.fi>"
  :license "MIT"
  :description "A simple namespace-aware XML writer for Common Lisp"
  :encoding :utf-8
  :components
  ((:module xmlw
    :pathname ""
    :components ((:file "package")
		 (:file "xmlw" :depends-on ("package"))))))
