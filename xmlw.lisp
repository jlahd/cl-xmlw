(in-package :xmlw)

;;; The namespace class

(defclass namespace ()
  ((uri :initarg :name :reader ns-uri)))

(defmethod print-object ((x namespace) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (prin1 (ns-uri x) stream)))

(defmethod make-load-form ((ns namespace) &optional environment)
  (declare (ignore environment))
  `(make-instance 'namespace :name ,(ns-uri ns)))

(defun ns= (ns1 ns2)
  (string= (ns-uri ns1) (ns-uri ns2)))

(defun namespace (name)
  (make-instance 'namespace :name name))

(defparameter *xmlns* (namespace "xmlns")
  "A namespace object denoting the 'xmlns' namespace.")

;;; XML stream management

(defvar *xml-stream*)
(defvar *xml-pretty*)
(defvar *xml-indent*)
(defvar *xml-ns-dict*)

(defun write-escaped (what &optional (stream *xml-stream*))
  "Write a string into XML output, escaping the characters <>&\"' with XML entities as necessary."
  (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
  (loop for ch across what
	do (case ch
	     (#\< (write-string "&lt;" stream))
	     (#\> (write-string "&gt;" stream))
	     (#\& (write-string "&amp;" stream))
	     (#\" (write-string "&quot;" stream))
	     (#\' (write-string "&apos;" stream))
	     (otherwise (write-char ch stream)))))

(defun write-name (n &optional (stream *xml-stream*))
  "Write a name - either a simple string or a namespace/string combination. Returns T if a fully qualified name has been written out, NIL if there is no namespace alias in the dictionary."
  (if (listp n)
      (let ((entry (find (first n) *xml-ns-dict* :key #'car)))
	(if entry
	    (prog1 t
	      (write-string (cdr entry) stream)
	      (write-char #\: stream)
	      (write-string (second n) stream))
	    (prog1 nil
	      (write-string (second n) stream))))
      (prog1 t
	(write-string n *xml-stream*))))

(defun write-attr (name value &optional (stream *xml-stream*))
  "Writes the name and value of an attribute."
  (write-char #\space stream)
  (unless (write-name name stream)
    (error "Namespace ~s not defined as alias, yet used for an attribute" (first name)))
  (write-char #\= stream)
  (write-char #\" stream)
  (write-escaped value stream)
  (write-char #\" stream))

(defun indent (delta &optional (stream *xml-stream*))
  "Indents, if necessary, to the correct indentation level."
  (when *xml-pretty*
    (multiple-value-bind (tabs spaces)
	(floor (+ *xml-indent* (* delta *xml-pretty*)) 8)
      (dotimes (i tabs)
	(write-char #\tab stream))
      (dotimes (i spaces)
	(write-char #\space stream)))))

;;; The XML output interface

(defvar *current-tag-name*)
(defvar *current-tag-attrs*)
(defvar *current-tag-opened*)
(defvar *current-tag-compact*)

(defmacro writing-xml ((stream &key (version "1.0") (encoding "UTF-8") standalone (indent 2) (prologue t))
		       &body body)
  "Main wrapper for writing out XML data. Writes the <?xml...?> tag followed by the XML document in body."
  `(let ((*xml-stream* ,stream)
	 (*xml-pretty* ,indent)
	 (*xml-indent* (and ,indent 0))
	 (*xml-ns-dict* `((,*xmlns* . "xmlns")))
	 (*current-tag-compact* ,(not indent)))
     (when ,prologue
       (write-string "<?xml" *xml-stream*)
       (write-attr "version" ,version)
       (write-attr "encoding" ,encoding)
       (when ,standalone
	 (write-attr "standalone" "yes"))
       (write-string "?>" *xml-stream*)
       (terpri *xml-stream*))
     ,@body))

(defun finish-open-tag (&key close)
  "After collecting the necessary information about a tag being opened, write it out now."
  (unless *current-tag-opened*
    (indent -1)
    (write-char #\< *xml-stream*)
    (unless (write-name *current-tag-name*)
      (write-attr "xmlns" (ns-uri (first *current-tag-name*))))
    (dolist (a (nreverse *current-tag-attrs*))
      (write-attr (car a) (cdr a)))
    (if close
	(progn
	  (when *xml-pretty*
	    (write-char #\space *xml-stream*))
	  (write-string "/>" *xml-stream*))
	(write-char #\> *xml-stream*))
    (when *current-tag-compact*
      (setf *xml-pretty* nil))
    (when *xml-pretty*
      (terpri *xml-stream*))
    (setf *current-tag-opened* t)))

(defmacro with-tag ((name &key compact) &body body)
  "Write out a tag. If compact is specified, the tag and all its contents will be written without line feeds and indentation also when pretty printing is enabled for the XML document."
  (when (and (listp name)
	     (or (not (first name))
		 (not (second name))
		 (cddr name)))
    (error 'type-error :expected-type '(list namespace string) :datum name))
  (let ((was-pretty (gensym)))
    `(progn
       (when (boundp '*current-tag-opened*)
	 (finish-open-tag))
       ,@(when (listp name)
	       `((unless (typep ,(first name) 'namespace)
		   (error 'type-error :expected-type 'namespace :datum ,(first name)))
		 (unless (stringp ,(second name))
		   (error 'type-error :expected-type 'string :datum ,(second name)))))
       (let ((*current-tag-name* ,(if (listp name)
				      `(list ,@name)
				      name))
	     (*xml-ns-dict* *xml-ns-dict*)
	     (*current-tag-attrs* nil)
	     (*current-tag-opened* nil)
	     (*current-tag-compact* (or *current-tag-compact* ,compact))
	     (*xml-indent* (and *xml-pretty* (+ *xml-indent* *xml-pretty*)))
	     (,was-pretty *xml-pretty*)
	     (*xml-pretty* (and (not *current-tag-compact*) *xml-pretty*)))
	 (multiple-value-prog1
	     (progn ,@body)
	   (if *current-tag-opened*
	       (progn
		 (indent -1)
		 (write-string "</" *xml-stream*)
		 (write-name *current-tag-name*)
		 (write-char #\> *xml-stream*)
		 (when ,was-pretty
		   (terpri *xml-stream*)))
	       (finish-open-tag :close t)))))))

(defun attr% (ns name value)
  "Helper function for writing attribute data."
  (unless (typep ns 'namespace)
    (error 'type-error :expected-type 'namespace :datum ns))
  (if (eq ns *xmlns*)
      (if (typep value 'namespace)
	  (progn
	    (push (cons value name) *xml-ns-dict*)
	    (push (cons (list ns name) (ns-uri value)) *current-tag-attrs*))
	  (error 'type-error :expected-type 'namespace :datum value))
      (if (stringp name)
	  (push (cons (list ns name) value) *current-tag-attrs*)
	  (error 'type-error :expected-type 'string :datum name)))
  value)

(defmacro attr (name value)
  "Write an attribute definition to the current tag."
  (when (and (listp name)
	     (or (not (first name))
		 (not (second name))
		 (cddr name)))
    (error 'type-error :expected-type '(list namespace string) :datum name))
  `(if *current-tag-opened*
       (error "attribute definitions must precede child tags (tag ~a)" *current-tag-name*)
       ,(if (listp name)
	    `(attr% ,(first name) ,(second name) ,value)
	    `(prog1 ,value
	       (push (cons ,name ,value) *current-tag-attrs*)))))

(defun content (&rest data)
  "Write content to the current tag."
  (finish-open-tag)
  (indent 0)
  (dolist (i data)
    (unless (stringp i)
      (error 'type-error :expected-type 'string :datum i))
    (write-escaped i))
  (when *xml-pretty*
    (terpri *xml-stream*))
  (first (last data)))

(defun content-format (&rest args)
  "Write formatted content to the current tag."
  (content (apply #'format nil args)))

(defmacro tag (name &rest content)
  "Write a compact tag with the given content."
  `(with-tag (,name :compact t)
     (content ,@content)))

(defun cdata (&rest data)
  "Write a CDATA block."
  (finish-open-tag)
  (indent 0)
  (write-string "<![CDATA[" *xml-stream*)
  (let ((esc 0))
    (dolist (i data)
      (unless (stringp i)
	(error 'type-error :expected-type 'string :datum i))
      (cond ((and (= esc 1)
		  (>= (length i) 2)
		  (string= (subseq i 0 2) "]>"))
	     (write-string "]]]><![CDATA[>" *xml-stream*)
	     (setf i (subseq i 2)))
	    ((and (= esc 2)
		  (>= (length i) 1)
		  (char= (char i 0) #\>))
	     (write-string "]]><![CDATA[" *xml-stream*)))
      (let ((start 0))
	(loop
	   (let ((next (search "]]>" i :start2 start)))
	     (if next
		 (progn
		   (incf next 2)
		   (write-string i *xml-stream* :start start :end next)
		   (write-string "]]><![CDATA[" *xml-stream*)
		   (setf start next))
		 (progn
		   (write-string i *xml-stream* :start start)
		   (return))))))
      (setf esc
	    (cond ((and (>= (length i) 2)
			(string= i "]]" :start1 (- (length i) 2)))
		   2)
		  ((and (>= (length i) 1)
			(char= #\] (char i (1- (length i)))))
		   1)
		  (t
		   0)))))
  (write-string "]]>" *xml-stream*)
  (when *xml-pretty*
    (terpri *xml-stream*))
  (first (last data)))

(defun cdata-format (&rest args)
  "Write a CDATA block, filling it with formatted data."
  (cdata (apply #'format nil args)))
