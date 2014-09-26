# Installation
Installation requires you have llvm-mc and bap-types installed.

$ git clone <repo>
$ make
$ make install

# Development
If you need to change anything, e.g., add files, change compiler flags, etc.,
you will need to regenerate the Makefile. We use the ocaml oasis project for
build management.  Do
$ opam install oasis
<edit _oasis with your additions>
$ oasis setup
$ make


