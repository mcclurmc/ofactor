Ofactor
=======

Description
-----------

Ofactor is a refactoring tool for OCaml. Currently it only supports
extraction of expressions to functions. Say you have a file thecode.ml
with a function:

    1: let myfunction arg1 arg2 =
    2:   let r = (arg1 * 4) + arg2 in
    3:   Printf.printf "Result is: %d\n" r

Perhaps we'd like to factor out the expression "(arg1 * 4) + arg2" to
its own function, to separate side effects from calculation. We could
call ofactor:

    ofactor extract -r l2c?:l2:c? -n calculate thecode.ml

Which would return:

    l1c0
    let calculate arg1 arg2 = (arg1 * 4) + arg2

Build Requirements
------------------

To build this program, just run make. You'll have to be sure you have
all of the following requirements -- especially OCaml 4.01
extension-points development branch! (This branch is basically meant
to provide a light-weight alternative to camlp4.) You can install this
compiler with opam using the command:

    opam switch 4.01.0dev+extension-points

opam is not required, but is _highly_ recommneded. Note that this
project doesn't actually use the new syntax attributes feature found
in the extension-points compiler version. This compiler branch was
chosen as a target because I wanted to follow along with the new
camlp4-alternative development, and because it contained a parse tree
mapper class which was convenient to use. Unfortunately, the parse
tree in this compiler version is incompatible with previous versions
of OCaml.

### The requirements

* OCaml 4.01.0+extension-points
* obuild
* oUnit
* cmldliner
* re
* ocamlfind

To do
-----

This project is by no means complete! There is a lot of work left to
be done, including adding new features as well as implementing more
cases of OCaml's syntax. Here is a short list:

* Emacs and Vim plugins for ofactor
* Implement remaining syntax cases for free and bound variables
* Function extraction from different structure items than values
* Inline function extraction
* Other refactoring operations:
  * Global identifier renaming
  * Module functorization

License
-------

This software is licensed under the ISC license. See LICENSE for the
exact terms of the license.
