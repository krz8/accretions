Accretions Manual
=================

[Homepage][home] [Manual][manual]

Accretions supplies collections (data structures) that aren't already
present in the Common Lisp standard, along with algorithms to operate
on those collections.  It hopes to be a little faster, maybe a little
more efficient, and maybe a little more consistent than other
implementations out there.

Accretions is typically used as a single package delivering a set of
container types (e.g., bags, red/black trees, ternary search trees),
and an orthogonal set of generic functions to manipulate them.  Rather
than different functions to implement similar functionality across
different types (e.g, MAP versus MAPC versus MAPHASH), there is just
one set of functions in Accretions that work on all the containers it
provides.  The use of CLOS generics minimizes the changes required in
client code as collection types change or are compared with one
another.

Accretions _also_ supports the ability to take even just a single data
collection to use separately in a resource constrained environment,
without dragging in all of the CLOS functionality (using structures*
instead of classes, regular functions instead of dispatched generics,
and so on).  In this context, each type is provided as a package whose
functions and other symbols can be accessed from your client code
directly.

> * Yup, many Common Lisp systems today implement structures on top of
> CLOS, or vice versa.  It used to be the case that structures, and
> their associated slot-access functions, were faster than CLOS and
> its generics.  I can't speak authoritatively about all the different
> Lisp systems out there, but I do believe the approach taken here
> _can_ be faster in some environments, but _should not_ be slower.

Accretions comes with a fairly complete test kit to verify its
functionality on your system.  If your build passes the tests,
you should be able to rely on Accretions in your application.
(**_Please_** [let me know][issues] if you find a situation where this
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

[home]:    https://krz8.github.io/accretions          "Accretions Homepage"
[manual]:  https://krz8.github.io/accretions/manual   "Accretions Manual"
[issues]:  https://github.com/krz8/accretions/issues  "Accretions Issues"



Accretions
----------

### Use

The author recommends the use of a “glue” package to add Accretions to
your code.  Certainly, you are free to introduce Accretions to your
application through other methods (package nicknames, package
imports), but the glue approach is recommended as it avoids annoying
naming conflicts.

1. Choose a name with which to refer to Accretions in your client
   software.  This name should be short and must be unique.  In the
   example below, we'll refer to the Accretions package with a glue
   package named **acr**.
1. Define that new package glue.  _Define no code in that package!_
   Instead, perform an import of the Accretions package, and re-export
   its symbols.  The **UIOP** package (included with **ASDF** 3.1.2
   and later) has a **define-package** form that improves on
   **defpackage** with several convenience forms, making this trivial
   below.
1. Prefix all references to Accretions symbols in your code with the
   new **acr** package.

```lisp
#-asdf3.1 (error "FOO requires ASDF 3.1.2 or later")

(uiop:define-package #:acr             ; these two lines are all you
  (:use-reexport #:accretions))        ; really need in your application

;;; using that package definition
(let ((things (acr:make :bag)))
  (gather-stuff things)
  (format t "found ~d items~%" (acr:size things))
  (unless (acr:emptyp things)
    (acr:map things #'fn)))
```


### Generic Functions

#### <strong>make</strong> type &key deepcopy shallowcopy

**make** creates and returns a new collection, according to the **type**
argument, which must be one of the following keywords:

**:bag** | Creates and returns a BAG object
**:tst** | Creates and returns a Ternary Search Tree object
**:rbt** | Creates and returns a Red-Black Tree object

Normally, a new empty collection is created and returned.  However,
the use of the :DEEPCOPY and :SHALLOWCOPY keywords will copy the
contents of an existing collection of a similar type.

**:deepcopy** | The new collection contains values that are typically EQUAL or EQUALP to the copied object, but no structure or contents are shared between the two collections
**:shallowcopy** | The contents and structure of the other object is shared with the newly created object

blah blah blah

<dl>
  <dt><strong>emptyp</strong> container</dt>
  <dd>Returns a true value if the supplied container is empty,
  otherwise returning NIL.</dd>

  <dt><strong>make</strong> kind</dt>
  <dd>Creates and returns a new container, according to the supplied
  keyword argument (one of <strong>:bag</strong>,
  <strong>:rbtree</strong>, or <strong>:tst</strong>).</dd>

  <dt><strong>map</strong> container function</dt>
  <dd>For every item present in the supplied container, the supplied
  function designator is invoked with that item as its argument.</dd>

  <dt><strong>size</strong> container</dt>
  <dd>Returns the number of entries in the supplied container.</dd>
</dl>



### Conditions




Bags
----

### Use

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

### Functions

add | Adds a new item to the supplied **bag**.
emptyp | Returns a true value if the supplied **bag** contains no items.
make | Creates and returns a new empty **bag** object.
map | For every item in the **bag**, the supplied function designator is called with the item as an argument.
size | Returns the number of items contained in the supplied **bag**.

