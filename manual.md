Accretions Manual
===============

Overview
--------

[Homepage][home] [Manual][manual]

Accretions supplies collections (data structures) that aren't already
present in the Common Lisp standard, along with algorithms to work on
those collections.  It hopes to be a little faster, maybe a little
more efficient, and maybe a little more consistent than other
implementations out there.

[home]:    https://krz8.github.io/accretions        "Accretions Homepage"
[manual]:  https://krz8.github.io/accretions/manual "Accretions Manual"


Accretions
----------

Bags
----

make | A function that returns a new empty bag object.
emptyp | Returns a true value if the supplied bag contains no items.
size | Returns the number of items contained in the supplied bag.
add | Adds a new item to the supplied bag.



Glue
----

Accretions is a package that is built out of many other smaller
packages, typically in a one-file-per-package style.  However,
sooner or later, a client (you!) runs into the situation where
the package name itself is simply too unwieldly to work with easily.
Consider the following example; the repeated use of the **accretions**
explicit package name, in order to access symbols therein, is just
a bit verbose.

```lisp
(let ((b (accretions:make-bag)))
  (gather-stuff b)
  (format t "found ~d items~%" (accretions:size b))
  (unless (accretions:emptyp b)
    (accretions:map b #'func)))
```

The first thing we might try would be to introduce a nickname for the
package.  For example, if we listed **acr** as a nickname for the
accretions package, the previous example becomes clearer.

```lisp
(let ((b (acr:make-bag)))
  (gather-stuff b)
  (format t "found ~d items~%" (acr:size b))
  (unless (acr:emptyp b)
    (acr:map b #'func)))
```

The trouble with nicknames is that it is impossible to predict
the packages on which client code depends or implements.  It's
entirely possible that there is already another **acr** package in
some client that we will conflict with.  The shorter the nickname,
the more likely this is to be a problem.

You might then turn to importing symbols from Accretions directly
into your project.  This works, and is often the clearest approach
of all, since no explicit packages are named… right up until you work
with a symbol that already exists in your project.  Or, perhaps, a
symbol that already exists in **COMMON-LISP**. Even if we adopted
a convention to never use any symbols that already exist in the
Common Lisp standard (a dubious convention at best, since we then
lose well-understood idioms such as **MAP** or resort to oddly spelled
variations), that will only solve the latter problem.  The former
problems, conflicts with client software, still lurk.

The cleanest approach I've found is the use of “glue” packages.
With a glue package, the *client* decides the name to use to reference
the symbols in the target package; not the package itself.
Only the author of the client software is in a position to know which
packages are going to be used in a project, so it makes sense to pick
a solution that puts control in the client author's hands.

Using an altenate form of package definition that's supplied with
UIOP, the glue that defines a new package **acr** that provides
access to all the exported symbols in the **accretions* package
looks like the following.

```lisp
#-asdf3.1 (error "FOO requires ASDF 3.1.2 or later")

(define-package #:acr
  (:use-reexport #:accretions))
```

With this approach, **acr** can be used as an explicit package name
for any symbol in the **accretions** package.  No code is compiled
in the **acr** package, it solely exists to bridge the gap between
client code and the required package. Plus, it can be easily
changed in the future, without changing nicknames or any other resources
in the Accretions system.

This really comes into its own if a client wants to use a single
collection from the Accretions package without dragging in the rest of
the library.  For example, imagine that there are resource constraints
of some sort that lead you to use the **bag** collection from
Accretions and nothing else.  The package in Accretions that defines
the **bag** functionality is named **accretions/src/bag**.

```lisp
(let ((b (accretions/src/bag:make-bag)))
  (gather-stuff b)
  (format t "found ~d items~%" (accretions/src/bag:size b))
  (unless (accretions/src/bag:emptyp b)
    (accretions/src/bag:map b #'func)))
```

Yikes.

But with just a little glue, this reduces to just the following.  In
some respects, this is even clearer than the previous solution.

```lisp
(define-package #:bag
  (:use-reexport #:accretions/src/bag))

(let ((b (bag:make)))
  (gather-stuff b)
  (format t "found ~d items~%" (bag:size b))
  (unless (bag:emptyp b)
    (bag:map b #'func)))
```

All of this is not to say that nicknames shouldn't be used, or that
importing symbols should be avoided, or anything else like that.
Those tools have their place in system development, to be sure.
Glue packages are just another approach to addressing the situation,
one that fits Accretions (and its clients) particularly well.

blah

blah

blah

Examples
--------

### Working with Accretions

### Working with a Single Container Implementation

Accretions is structured in a way that makes it easy to adopt just a single
container into your project without requiring the entire system.  This is
intended for resource-constrained situations, where the full Accretions
system might be too large for your purposes.  To work with it in that
manner:

First, select the container of interest.  For example, let's say you only
need to work with **bags.**  In this case, the source file from Accretions
that you need is **src/bags.lisp.**  Make that file a part of your project.

When you peek inside, you'll find that the file defines a package named
**accretions/src/bag**.  Obviously, no one in their right mind would expect
you to type such a monter prefix every time.  It's also _not recommended_
to simply import the package into your project; there are times when symbols
in the package will shadow other symbols in your project, or perhaps even in
the **COMMON-LISP** package.  Instead, a clean way to address this is to
introduce a “glue” package.

```lisp
(uiop



blah blah blah


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

:PEEK will return the same values as a simple invocation of the
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

:RESET will move the iterator back to the beginning of the
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


### Testing Presence

CONTAINSP searches a collection for a particular item (or, in a
key/value store, a particular key) and returns true when at least one
is found.

    CL-USER> (let ((b (acr:make-bag)))
               (mapc (lambda (x) (acr:add x b))
                     '("foobar" t 42))
               (print (acr:containsp "foo" b))
               (print (acr:containsp "foobar" b)))
    NIL
    T



Bags
----

Also known as multisets, a bag is a simple unordered collection of things.
Any value may be stored in a bag, including NIL.  Multiple instances of
those


Ternary Search Tries
--------------------


Red-Black Balanced Trees
------------------------
