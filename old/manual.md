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
another.  Conditions are adopted as the general error reporting
facility, rather than return codes.

**NOTE**: Don't let that bother you much.  Errors and failures are
  different things, and thus you aren't likely to encounter error
  conditions at all.  For example, looking up GOO in a list of FOO BAR
  BAZ is a failure, but not an error.

Accretions _also_ supports using any data collection type directly in
a resource constrained environment, without dragging in all of the
CLOS functionality (e.g., using structures instead of classes, regular
functions instead of dispatched generics, eschewing conditions, and so
on).  In this context, each type is provided as a package whose
functions and other symbols can be accessed from your client code
directly.  If you look closely, you'll find that the low-level
packages, such as src/bag.lisp, are effectively standalone and can be
accessed without any other dependencies.  Most people won't need this
and for them, the CLOS functionality in Accretions will be fine; but
for those of you chasing CPU cycles and memory, this should help.

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

### Getting Accretions Into Your Project

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


### Collection Types

The collections supplied by Accretions fall into one of two classes.

**value-collection**
Some collections are simply made up of single values, typically added
one at a time through ADD or some other mechanism.  For example,
a Bag or a Stack are simple value-collections.

**kv-collection**
Other collections are made up of item pairs, a key and its value.
Dictionaries and hash tables are examples of kv-collections.

In the descriptions below, you'll see one or the other of these when
a distinction in functionality has to be made.  Other times, you'll
simply see **collection**, indicating that the function works identically
for both kinds of collections.


### Functions

#### <strong>make</strong> type

Creates and returns a new collection, according to the **type**
argument, which must be one of the following keywords:

**:bag** | Creates and returns a Bag object
**:tst** | Creates and returns a Ternary Search Tree object
**:rbt** | Creates and returns a Red-Black Tree object


#### <strong>copy</strong> collection

Given an Accretions collection, this function returns a new
_shallow copy_ of that collection.  By shallow, it is meant
that the collection itself is new, but any keys or values
stored within it are shared between the source and returned
collections.

Because Accretions is designed to work with the widest range of keys
and values, it is impossible to implement a general “deep copy” (aka
clone) function; writing such requires _a priori_ knowledge of the
items stored within the collection.  However, for any given
application, it should be easy to construct a clone function that
suits your use of Accretions in your project.


#### <strong>add</strong> value-collection value

Add the supplied VALUE to the supplied COLLECTION.


#### <strong>add</strong> kv-collection key value

Associates the supplied KEY with a supplied VALUE in the supplied
COLLECTION.


#### <strong>mapfun</strong> value-collection function

Corresponds to standard CL functions such as MAPC and MAP.
For every item in the supplied COLLECTION, the supplied
FUNCTION designator is called once with that item as its sole
argument.  Return values are not considered.  If you need to
exit early before the entire collection is processed, use a local
BLOCK and RETURN.  For example,

```lisp
(defun containsp (collection thing)
  (block nil
    (mapfun collection (lambda (x) (when (eql thing x)
                                     (return t))))
    nil))
```


### <strong>mapfun</strong> kv-collection function

The key-value variant of MAPFUN.  Here, the supplied FUNCTION is
called with two arguments for every entry in the supplied COLLECTION.
The first argument is a key, and the second argument is the
value associated with that key, as they appear in the COLLECTION.


#### <strong>size</strong> collection

Returns an unsigned byte indicating the number of items in the supplied
COLLECTION.  In this context, an “item” corresponds to a previous call
to the **add** function.  Hence, for a Bag this might be a single value,
while for a tree it might be a key/value pair.


#### <strong>emptyp</strong> collection

Returns true when the supplied collection contains no items of any kind.



#### <strong>bagp</strong> value

Returns a true value when the supplied VALUE is an Accretions Bag;
otherwise, returns NIL in all other cases.



Blah Blah Blah
--------------

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

