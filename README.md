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

[home]:    https://krz8.github.io/accretions        "Accretions Homepage"
[manual]:  https://krz8.github.io/accretions/manual "Accretions Manual"



Functionality
-------------

### What You Get

#### Structures

- Bags (Multisets): A simple un-ordered collection of items.

- Ternary Search Trees: A really nice data structure somewhere between
  hash tables and tries.  Both performance and speed is comparable to
  hash tables, but they have unique properties much like indexed tries
  without the space overhead (e.g., you can store Unicode strings in
  them without wasting memory).  They are ideal for managing
  dictionaries to implement sound-alike words and partial matches (as
  in auto-correct applications).

- Red-Black Trees

- more as I need them and study them; feel free to suggest something!

#### API

- Common Generics: All collections are manipulated through the same
  set of CLOS generic functions.  We aren't maintaining compatibility
  with other lisp systems (as Common Lisp must), so we are free to
  write common operations for the different operations.  Just one
  interface serves many data structures.

- something about inevitable package symbol name collisions


### What's Supported Currently

Eventually, I'll request getting this package added to [Quicklisp][],
when I feel Accretions is good enough.

[Quicklisp]: https://www.quicklisp.org/beta/ "The Quicklisp Project Homepage"


### What's Not Supported Currently

I've experimented with functional data structures (as described by
Osazaki) and it's true, they have very promising characteristics in
persistence, safety, and time.  However, their overhead in space can
be prohibitive for larger applications, and so I'll revisit this
another time.



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
