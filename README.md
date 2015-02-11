# cl-xmlw

A simple namespace-aware XML writer for Common Lisp. Much like the
xml-emitter project (http://www.cliki.net/xml-emitter), but with
better support for characters with codes over 255, namespaces, and CDATA.

## Basic use

Without extra options, you get prettily printed XML with standard stuff:

```cl
(xmlw:writing-xml (*standard-output*)
  (xmlw:with-tag ("foo")
    (xmlw:attr "bar" "baz")
    (xmlw:content "Hello world!")))
```
```xml
<?xml version="1.0" encoding="UTF-8"?>
<foo bar="baz">
  Hello world!
</foo>
```

You can disable pretty printing and set the <?xml ...?> attributes with certain keyword arguments:

```cl
(xmlw:writing-xml (*standard-output* :indent nil :standalone t)
  (xmlw:with-tag ("foo")
    (xmlw:attr "bar" "baz")
    (xmlw:content "Hello world!")))
```
```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?><foo bar="baz">Hello world!</foo>
```

Marking a tag compact removes the pretty printing from that tag, regardless of the whole document's settings:

```cl
(xmlw:writing-xml (*standard-output* :indent 4 :prologue nil)
  (xmlw:with-tag ("foo")
    (xmlw:attr "bar" "baz")
    (xmlw:with-tag ("simple" :compact t)
      (xmlw:content "simple content"))
    (xmlw:with-tag ("complex")
      (xmlw:cdata "extremely complex"))))
```
```xml
<foo bar="baz">
    <simple>simple content</simple>
    <complex>
	<![CDATA[extremely complex]]>
    </complex>
</foo>
```

For convenience, the previous example can be written shortly using the `tag` macro:

```cl
(xmlw:writing-xml (*standard-output* :indent 4 :prologue nil)
  (xmlw:with-tag ("foo")
    (xmlw:attr "bar" "baz")
    (xmlw:tag "simple" "simple content")
    (xmlw:with-tag ("complex")
      (xmlw:cdata "extremely complex"))))
```
```xml
<foo bar="baz">
    <simple>simple content</simple>
    <complex>
	<![CDATA[extremely complex]]>
    </complex>
</foo>
```

Escaping of the special characters is done automatically:

```cl
(xmlw:writing-xml (*standard-output* :indent 4)
  (xmlw:with-tag ("foo")
    (xmlw:attr "bar" "i'm a so-called \"value\".")
    (xmlw:content "42 > 17 && 17 < 42")
    (xmlw:cdata "ASCII fish, like ]]>, taste good.")))
```
```xml
<?xml version="1.0" encoding="UTF-8"?>
<foo bar="i&apos;m a so-called &quot;value&quot;.">
    42 &gt; 17 &amp;&amp; 17 &lt; 42
    <![CDATA[ASCII fish, like ]]]]><![CDATA[>, taste good.]]>
</foo>
```

## Namespaces

If you only need a namespace for a single tag, you can specify it there:

```cl
(xmlw:writing-xml (*standard-output*)
  (xmlw:with-tag (((namespace "http://somwhere/quux") "foo"))
    (xmlw:attr "bar" "baz")
    (xmlw:content "Hello world!")))
```
```xml
<?xml version="1.0" encoding="UTF-8"?>
<foo xmlns="http://somwhere/quux" bar="baz">
  Hello world!
</foo>
```

However, usually you would reuse the namespaces for a more complex structure:

```cl
(let ((quux (namespace "http://somwhere/quux"))
      (xyzzy (namespace "http://somewhere/xyzzy")))
  (xmlw:writing-xml (*standard-output*)
    (xmlw:with-tag ((quux "foo"))
      (xmlw:attr "bar" "baz")
      (xmlw:with-tag ((xyzzy "greeting"))
	(xmlw:content "Hello world!")))))
```
```xml
<?xml version="1.0" encoding="UTF-8"?>
<foo xmlns="http://somwhere/quux" bar="baz">
  <greeting xmlns="http://somewhere/xyzzy">
    Hello world!
  </greeting>
</foo>
```

And to get the tags (and attributes) prefixed nicely:

```cl
(let ((quux (namespace "http://somwhere/quux"))
      (xyzzy (namespace "http://somewhere/xyzzy")))
  (xmlw:writing-xml (*standard-output*)
    (xmlw:with-tag ((quux "foo"))
      (xmlw:attr (xmlw:*xmlns* "quuxref") quux)
      (xmlw:attr (xmlw:*xmlns* "xyzzy") xyzzy)
      (xmlw:attr (quux "bar") "baz")
      (xmlw:with-tag ((xyzzy "greeting"))
	(xmlw:attr (quux "target") "mankind")
	(xmlw:attr (xyzzy "kind") "benevolent")
	(xmlw:content "Hello world!")))))
```
```xml
<?xml version="1.0" encoding="UTF-8"?>
<quuxref:foo xmlns:quuxref="http://somwhere/quux" xmlns:xyzzy="http://somewhere/xyzzy" quuxref:bar="baz">
  <xyzzy:greeting quuxref:target="mankind" xyzzy:kind="benevolent">
    Hello world!
  </greeting>
</foo>
```

## Reference

### Macro `WRITING-XML`

**(writing-xml stream &key (version "1.0") (encoding "UTF-8") standalone (indent 2) (prologue t) &body body)**

* *stream* (stream) specifies the stream for output
* *version* (string) contains the value of the version attribute in the prologue
* *encoding* (string) contains the vlaue of the encoding attribute in the prologue. Note that it does NOT have any effect on encoding of the content; that is left for the output stream to handle. Thus, you should match the value of the *encoding* parameter with your output stream's encoding.
* if *standalone* (boolean) is set to a true value, the prologue will contain the `standalone="yes"` attribute
* *indent* (`NIL` or integer) specifiees the number of spaces of indentation. Set to `NIL` to disable indentation and line breaks totally.
* set *prologue* (boolean) to `NIL` to disable writing of the `<?xml ...?>` prologue.
* The *body* contains the code for writing out the actual document.

### Macro `WITH-TAG`

**(with-tag (name &key compact) &body body)**
**(with-tag ((ns name) &key compact) &body body)**

* *name* (string) is the tag's name.
* *ns* (namespace) is the tag's namespace.
* *compact* (boolean) can be set to true to disable indentation and line feeds for this tag's content.
* *body* contains the attributes, other tags, and content for the tag.

### Macro `ATTR`

**(attr name value)**
**(attr (ns name) value)**

* *name* (string) is the attribute's name.
* *ns* (namespace) is the attribute's namespace.
* *value* (string) is the attribute's value.

### Function `CONTENT`

**(content &rest data)**

The *data* (strings) are written, escaped as necessary, into the document.

### Function `CDATA`

**(cdata &rest data)**

The *data* (strings) are written, escaped as necessary, into the document, inside a `<![CDATA[...]]>` block.

### Function `CONTENT-FORMAT`

**(content-format &rest args)**

A convenience function - identical to `(content (format nil [args]))`.

### Function `CDATA-FORMAT`

**(cdata-format &rest args)**

A convenience function - identical to `(cdata (format nil [args]))`.

### Macro `TAG`

**(tag name &rest content)**
**(tag (ns name) &rest content)**

* *name* (string) is the tag's name.
* *ns* (namespace) is the tag's namespace.
* *content* (strings) contain the tag's content.

### Macro `TAG-FORMAT`

**(tag-format name &rest args)**
**(tag-format (ns name) &rest args)**

A convenience function - identical to `(tag name (format nil [args]))`.
