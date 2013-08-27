Changes
=======

Version 0.2.1
-------------
Dummy release to force rebuild on hackage (now that haskell-src-exts 1.14 is
released).

Version 0.2
-----------

* Compiler now accepts the `--package-name` argument, to specify the name of the
  package being compiled. `CompileFn` is changed accordingly.
* Add support for relative paths in package databases.
