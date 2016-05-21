# Exercises for the book "Parallel and Concurrent Programming in Haskell"

This project adds some exercises for the different topics presented in the book
"Parallel and Concurrent Programming in Haskell", by Simon Marlow. The
exercises in this repository are taken from the examples of the book, but there
is no other relation between this repository and the book author.

The idea of this repository is that you can try to implement the different
functions described in the book by yourself before reading the author's
solutions. To help in this purpose, there are unit tests that should succeed
once the functionality is implemented.

To be able to build and run the tests, you need to have
[Stack](https://github.com/commercialhaskell/stack) installed in your machine.
Once `stack` is installed in the system, the tests for a given book chapter can
be run by going to the desired directory and executing:

    stack test

After implementing the functions as prescribed in the book, the tests should
pass.
