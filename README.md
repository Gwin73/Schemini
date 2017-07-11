# Schemini
Toy interpreter of a minimal scheme(ish) language, in around 250 lines of Haskell (one file version, without optional type declarations and newlines). Bugfree? Revolutionary? Useful in any way? 3*no, but a fun first Haskell project with lots of monads. 

Schemini supports ints, bools, strings, lamdas, unsafe unhygienic macros, proper lists, quotes, ifs, defs (but no sets), loading of other scm files, some functions and a few library functions and macros.

To run the example.scm (using ghc and windows) compile with "ghc -o interpreter .\interpreter.hs" and interpret with ".\interpreter.exe "example.scm"".
