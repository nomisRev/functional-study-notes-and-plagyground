# Chapter 1 - What is functional programming?

* Functional programming is based on a simple premise with far-reaching implications: we construct our programs using only pure functions,
in other words functions that have no side-effects.

* A function f with input type A and output type B is a computation that relates every value a: A to exactly one value b: B such that b is determined solely by the value of a. Any changing state of an internal or external process is irrelevant to computing the result f(a)

* A pure function is modular and composable because it separates the logic of the computation itself from “what to do with the result” and “how to obtain the input”; it’s a black box. Input is obtained in exactly one way: via the argument(s) to the function. And the output is simply computed and returned.

## What is a side effect?

* A side effect is doing something else than just returning/calculating a value.
* Examples:
    * Modifying a variable
    * Modifying a data structure in place
    * Setting a field on an object
    * Throwing an exception or halting with an error
    * Printing to the console or reading user input
    * Reading from or writing to a file
    * Drawing on the screen

## Referential transparency

* An expression e is referentially transparent if, for all programs p , all occurrences of e in p can be replaced by the result of evaluating e without affecting the meaning of p. A function f is pure if the expression f(x) is referentially transparent for all referentially transparent x.

### Example with substition model
A “modified” string is really a new string and the old string remains intact. String is an immutable object, reverse is a function of type `String => String`

```scala
val x = "Hello, World"
val res1 = x.reverse  //dlorW ,olleH
val res2 = x.reverse //dlorW ,olleH
```

Suppose we replace all occurrences of the term x with the expression referenced by x.
```scala
val res1 = "Hello, World".reverse  //dlorW ,olleH
val res2 = "Hello, World".reverse //dlorW ,olleH
```
This transformation doesn’t affect the outcome. The values of r1 and r2 are the same as before, so x was referentially transparent.
