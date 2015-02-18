(in-package #:cl)
(defpackage #:xmlw-system (:use #:asdf #:cl))
(in-package #:xmlw-system)

(asdf:defsystem cl-xmlw
  :name "cl-xmlw"
  :version "1.0.0"
  :author "Jussi Lahdenniemi <jussi@lahdenniemi.fi>"
  :maintainer "Jussi Lahdenniemi <jussi@lahdenniemi.fi>"
  :license "MIT"
  :description "A simple namespace-aware XML writer for Common Lisp"
  :encoding :utf-8
  :components
  ((:module cl-xmlw
    :pathname ""
    :components ((:file "package")
		 (:file "cl-xmlw" :depends-on ("package"))))))
