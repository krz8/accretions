Accretions Manual
=================

[Homepage][home] [Manual][manual]

Accretions supplies collections (data structures) that aren't already
present in the Common Lisp standard, along with algorithms to work on
those collections.  It hopes to be a little faster, maybe a little
more efficient, and maybe a little more consistent than other
implementations out there.

Accretions is organized a single package delivering a set of container
types (e.g., bags, red/black trees, ternary search trees), and an
orthogonal set of generic functions to manipulate them.  Rather
than different functions to implement similar functionality across
different types (e.g, MAPC versus MAPHASH), there is just one set
of functions in Accretions that work on all the containers it provides.
The heavy use of CLOS generics is meant to minimize the changes
required in client code as collection types change or are compared with
one another.

Accretions _also_ supports the ability to take even just a single
data collection to use separately in a resource constrained
environment, without dragging in all of the CLOS functionality.
In this context, “regular” (non-generic) functions are provided
to work directly with each type, avoiding object dispatch overhead.

Accretions comes with a fairly complete test kit to verify its
functionality on your system.  If your build passes the tests,
you should be able to rely on Accretions in your application.
(**_Please_** [let me know][krz8] if you find a situation where this
isn't the case!)

The ability to “store anything” is maintained across Accretions.
Thus, NIL can be used for both values and even keys (in the right
contexts).  Common Lisp's convention of using multiple return values
is used in Accretions to differentiate between returning a
previously-stored NIL value and end-of-iteration situations.  With
this flexibility comes some responsibility on the caller: for example,
not just characters and strings, but in fact any element and sequence
can be used for keys in both trees and tries.  Therefore, sane test
functions must be supplied by the caller lest the comparisons become
nonsensical.

[home]:    https://krz8.github.io/accretions        "Accretions Homepage"
[manual]:  https://krz8.github.io/accretions/manual "Accretions Manual"
[krz8]:    https://github.com/krz8                  "krz8 homepage"



Accretions
----------

=== Use ===

The author recommends the use of a “glue” package to add Accretions to
your code.  Certainly, you are free to introduce Accretions to your
application through other methods (package nicknames, package
imports); this approach is recommended here as it avoids annoying
naming conflicts.

1. Choose a name for the Accretions package that you'll use in your
   client software.  This name should be short and must be unique.
   In the example below, we'll refer to the Accretions package with
   a glue package named **acr**.
1. Define that new package glue.  **Define no code in that package!**
   Instead, perform an import (allowing shadowing) of the Accretions package,
   and re-export its symbols.  The **UIOP** package (included with
   **ASDF** 3.1.2 and later) has a **define-package* form that improves
   on **defpackage** with several convenience forms, making this trivial.
1. Prefix all references to Accretions symbols in your code with the
   **acr** package.

```lisp
#-asdf3.1 (error "FOO requires ASDF 3.1.2 or later")

(uiop:define-package #:acr             ; these two lines are all you
  (:use-reexport #:accretions))        ; really need in your application

;;; using that package definition
(let ((b (acr:make :bag)))
  (gather-stuff b)
  (format t "found ~d items~%" (acr:size b))
  (unless (acr:emptyp b)
    (acr:map b #'fn)))
```


=== Generic Functions ===

<dl>
  <dt>**emptyp** _container_</dt>
  <dd>Returns a true value if the supplied container is empty,
  otherwise returning NIL.</dd>

  <dt>**make** _kind_</dt>
  <dd>Creates and returns a new container, according to the
  supplied keyword argument (one of **:bag**, **:rbtree**,
  **:tst**).</dd>

  <dt>**map** _container_ _function_</dt>
  <dd>For every item present in the supplied container, the supplied
  function designator is invoked with that item as its argument.</dd>

  <dt>**size** _container_</dt>
  <dd>Returns the number of entries in the supplied container.</dd>
</dl>



=== Conditions ===




Bags
----

=== Use ===

Instead of using the full Accretions package (as seen above), you
can use only the **bag** container, leaving the rest of Accretions
behind.  This is useful in certain resource-constrained environments.
While CLOS is still used, it only provides a definition of the bag
type itself; no generic functions or conditions are used in this
context.

The author recommends the use of a “glue” package to add **bag** to
your code.  Certainly, you are free to introduce this package to your
application through other methods (package nicknames, package
imports); this approach is recommended here as it avoids annoying
naming conflicts.

1. Choose a name for the bag package that you'll use in your
   client software.  This name should be short and must be unique.
   In the example below, we'll refer to the bag package with
   a glue package named **bag**.
1. Define that new package glue.  **Define no code in that package!**
   Instead, perform an import (allowing shadowing) of the bag package,
   and re-export its symbols.  The **UIOP** package (included with
   **ASDF** 3.1.2 and later) has a **define-package* form that improves
   on **defpackage** with several convenience forms, making this trivial.
1. Prefix all references to Accretions symbols in your code with the
   **acr** package.

```lisp
#-asdf3.1 (error "FOO requires ASDF 3.1.2 or later")

(uiop:define-package #:bag                ; these two lines are all you
  (:use-reexport #:accretions/src/bag))   ; really need in your application

;;; using that package definition
(let ((b (bag:make)))
  (gather-stuff b)
  (format t "found ~d items~%" (bag:size b))
  (unless (bag:emptyp b)
    (bag:map b #'fn)))
```

=== Functions ===

add | Adds a new item to the supplied **bag**.
emptyp | Returns a true value if the supplied **bag** contains no items.
make | Creates and returns a new empty **bag** object.
map | For every item in the **bag**, the supplied function designator is called with the item as an argument.
size | Returns the number of items contained in the supplied **bag**.

