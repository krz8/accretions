Accretions Manual
===============

Overview
--------

[Homepage][home] [Manual][manual]

Accretions is a collection of data structures that don't already exist
in standard Common Lisp.  Of course, we already have lists, and most
CL implementations also have performant hash tables.  But there remain
other data structures that are essential to certain applications.
Accretions is my collection of them.

[home]:    https://krz8.github.io/accretions        "Accretions Homepage"
[manual]:  https://krz8.github.io/accretions/manual "Accretions Manual"


General Notes
-------------

Accretions is organized as a set of data collections and an orthogonal
set of generic functions to manipulate them.  This is intentional;
rather than using different functions to work through different
collections (e.g., MAPC versus MAPHASH), there is one set of functions
that apply to all the different collections (as much as reasonable).
“Reasonable” is meant to acknowledge that some functions are
necessarily unique to a specific collection.

While “collection” isn't my favorite word, it is somewhat standard
jargon in the industry.  Accretions, then, uses it as the name of all
the containing data types it provides (e.g., bags, tries, trees).

The ability to “store anything” is maintained across Accretions.
Thus, NIL can be used for both values and even keys (in the right
contexts).  Common Lisp's convention of using multiple return values
is used in Accretions to differentiate between returning a
previously-stored NIL value and end-of-iteration situations.  With
this flexibility comes some responsibility on the caller: for example,
not just characters and strings, but in fact any element and sequence
can be used for keys in both trees and tries.  However, sane test
functions must be supplied by the caller lest the comparisons become
nonsensical.

An effort has been made to avoid any naming conflicts with the rest of
Common Lisp.  However, I don't recommend importing the symbols from
Accretions into your project; sooner or later, it'll conflict with
something you've written yourself, or something that appears in
another library.  Therefore, while I feel it's perfectly reasonable to
place "accretions" in the :DEPENDS-ON clause of your ASDF file, I
would not employ the :USE mechanism of your DEFPACKAGE form for
Accretions.  Instead, use the very short package nickname ACR when
calling functionality from Accretions.


API
---

### Creation

All data collections have a specific function to create a new instance
(e.g., MAKE-BAG).  For example:

    (let ((b (acr:make-bag)))
      (assert b)
      …)

- MAKE-BAG creates and returns a new bag.
- MAKE-COUNTED-BAG creates and returns a new bag that tracks the
  number of items it contains.

### Non-destructive Tests

EMPTYP returns true if the supplied collection is empty (i.e.,
contains no items).

    (let ((b (acr:make-bag)))
      (assert (acr:emptyp b))
      …)

SIZE can be used to fetch the number of items currently stored in a
collection.  This only works for those collections that are counted.
Not all collections manage a size, but there is always a variant that
does (e.g., bags versus counted bags).

    (let ((b (acr:make-counted-bag)))
      (assert (acr:emptyp b))
      (assert (zerop (acr:size b)))
      (assert (acr:add "foobar" b))
      (assert (acr:add t b))
      (assert (acr:add 42 b))
      (assert (acr:add nil b))
      (assert (= (acr:size b) 4))
      …)

### Adding Items

ADD is used to add a new item to the supplied collection.  In addition
to the item and the collection that must be provided, an optional
argument WHERE may sometimes be supplied to specify alternative
locations to add the new item, when supported by the collection.  For
example, while a bag only supports adding items, a queue might support
adding items at its head and tail with the head being the default
location.

    (let ((q (acr:make-deque)))
      (acr:add "foo" q)
      (acr:add "bar" q)
      (acr:add "baz" q :tail)
      …)

### Mapping Functions

MAPFUN implements the common idiom of invoking a caller-supplied
function (lambda form) over each item in a collection.  This is
handled by MAPHASH and MAPC and MAP in Common Lisp, depending on the
underlying data type; in Accretions, the same function MAPFUN is
commonly used for all collections.

    CL-USER> (let ((b (acr:make-bag)))
      	       (mapc (lambda (x) (acr:add x b))
               	     '("foobar" t 42))
	       (acr:mapfun (lambda (x) (print x))
      	               b))
    42
    T
    "foobar"

A caller-supplied function (or lambda form) can be called over each
item stored in a collection.  Regardless of the collection, the same
function MAPFUN

### Iteration

An iterator is presented as a generating function in Accretions; each
call of the iterator returns another item in the collection until no
more are available.  Multiple return values are used to indicate when
no more values from the collection are available.

WITH-ITERATOR is a simple macro that wraps the functionality of
MAKE-ITERATOR into a commonly useful form, binding the returned
iterator function to a given name for invocation.

    CL-USER> (let ((bag (acr:make-bag)))
	       …
	       (acr:with-iterator (fn b)
	         (loop with item and ok
		       do (setf (values item ok) (fn))
		       while ok
		       do (print item)))
    "foobar"
    T
    42

As seen above, the primary return function from the iterator is,
naturally, an item from the collection.  The second value indicates
whether the primary value is from the collection, or if all items have
been seen through the iterator.  This is handy when, for example, NIL
has been added to a collection.  The same pattern is implemented in
Common Lisp's GETHASH.

MAKE-ITERATOR directly returns the function used in WITH-ITERATOR.
The two forms shown here are roughly equivalent.

    (acr:with-iterator (fn bag)
      (fn))
    
    (let ((f (acr:make-iterator bag)))
      (funcall f))

Iterators also take optional modifiers to their behavior as a trailing
keyword:

- :PEEK will return the same values as a simple invocation of the
  iterating function, but it will not advance.

    CL-USER> (let ((b (acr:make-bag)))
		   (mapc (lambda (x) (acr:add x b))
			 '("foobar" t 42))
		   (acr:with-iterator (fn b)
		     (print (fn :peek))
		     (print (fn))
		     (print (fn))))
    42
    42
    T

- :RESET will move the iterator back to the beginning of the
  collection, leaving the iterator in the same state as if it had been
  newly created.

    CL-USER> (let ((b (acr:make-bag)))
		   (mapc (lambda (x) (acr:add x b))
			 '("foobar" t 42))
		   (acr:with-iterator (fn b)
		     (print (fn))
		     (print (fn))
		     (fn :reset)
		     (print (fn))))
    42
    T
    42


Bags
----

Also known as multisets, a bag is a simple unordered collection of things.
Any value may be stored in a bag, including NIL.  Multiple instances of
those


Ternary Search Tries
--------------------


Red-Black Balanced Trees
------------------------
