Accretions
========

Overview
--------

[Homepage][home] [Manual][manual]

Accretions is a collection of data structures that aren't present in
standard Common Lisp.  Of course, we already have lists, and most CL
implementation also have performant hash tables.  But there remain
other data structures that are essential to certain applications.
Accretions is my collection of them.

Accretions would not exist without the essential work of Donald Knuth,
Robert Sedgewick, Jon Bentley, Doug Hoyte, and others.

[home]:    https://krz8.github.io/accretions         "Accretions Homepage"
[manual]:  https://krz8.github.io/accretions/manual  "Accretions Manual"



Functionality
-------------

### What You Get

#### Collections

- Bags (aka Multisets): A simple un-ordered collection of values.

- Ternary Search Trees: A really nice collection somewhere between
  hash tables and tries.  The implementation here allows for any
  key sequence (not just strings), and supports all the functionality
  of a trie without the unfortunate space overhead.

  Aside: The widespead adoption of Unicode, while a good thing, killed
  much of the use of trie structures in memory.  One widely adopted
  approach to dealing with Unicode in trie structures was to maintain
  strings in an encoding like UTF-8 rather than native codepoints.
  Here, the use of ternary search trees instead of tries is a much
  more convenient solution (imho).

- Red-Black Trees

  Notes on 2-3 and red-black uses, etc

- Alternate hashing algorithms, and the hash tables that go with
  them.

  Dig up that survey.  Give a range of options, including algorithms
  subsequent to CityHash and others.  Since we can't affect existing
  functionality in CL, we'll invert: adopt native hashes in Accretions
  and support others as options.

- more as I need them and study them; feel free to suggest something!

#### API

At its top level, Accretions provides a package that grants access to
all the collections and algorithms listed above.  Generic functions
are heavily leveraged such that different collections can be swapped
and evaluated without change much client code at all.  Sometimes, in
fact, all that's required is merely setting a flag.

Additionally, the individual collections and their functionality in
Accretions is also available directly, without the overhead of CLOS
generic functions.  CLOS is still used for the data structures themselves,
but manipulations are carried out with regular (non-generic) functions.
These are intended for resource-constrained environments; pulling in
the entire Accretions package might be a headache when all one needs
are **bags**.



### What's Supported Currently

Eventually, I'll request getting this package added to [Quicklisp][],
when I feel Accretions is good enough.

[Quicklisp]: https://www.quicklisp.org/beta/ "The Quicklisp Project Homepage"



### What's Not Supported Currently

I've experimented with functional data structures (as described by
Osazaki) and it's true, they have very promising characteristics in
persistence, safety, and time.  However, their (albiet temporary)
overhead in space can be prohibitive for larger applications, and so
I'll revisit this another time.



### Future Work



License
-------

Accretions is available under the [MIT License][].

Copyright © 2018 Robert S. Krzaczek

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
“Software”), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

[MIT License]: https://opensource.org/licenses/MIT
               "The MIT Open Source License"
