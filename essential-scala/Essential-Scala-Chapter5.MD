# Chapter 5 Sequencing Computations


**Question:** Is it more convenient to rewrite methods in terms of fold if they were implemented using pattern matching or polymorphic? What does this tell us about the best use of fold?

**Answer/Opinion:** Implementing fold with pattern matching is more convenient. Because the logic is in one method instead of spread out over different methods in different classes. This improves readability.

```scala
def fold[B](identity: B, f: (A, B) => B): B = this match {
    case End() => identity
    case Pair(hd, tl) => f(hd, tl.fold(identity, f))
}
```
```scala
final case class End[A]() extends LinkedList[A] {
  override def fold[B](identity: B, f: (A, B) => B): B = identity
}
final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A] {
  override def fold[B](identity: B, f: (A, B) => B): B = f(head, tail.fold(identity, f))
}
```

**Fold is just an adaptation of structural recursion where we allow the user to pass in the functions we apply at each case. It is the generic transformation or iteration method.**

## Fold pattern
For an algebraic datatype A , fold converts it to a generic type B . Fold is a structural recursion with:
* One function parameter for each case in A; i.e. 2 parameters for LinkedList
* Each function takes as parameters the fields for its associated class; i.e. () => B and (A,B) => B for LinkedList
* if A is recursive, any function parameters that refer to a recursive field take a parameter of type B; i.e. End is not recursive, Pair is recursive: (A,B) => B

The right-hand side of pattern matching cases, or the polymorphic methods as appropriate, consists of calls to the appropriate function.

#### placeholder syntax
`_` is a placeholder in Scala; It can be used in pattern matching to mean any value (or an ignored value) or more powerful it can be used to shorten lambda syntax when types can be inferred.

```scala
_ + _ // expands to `(a, b) => a + b`
foo (_) // expands to `(a) => foo(a)`
foo (_, b) // expands to `(a) => foo(a, b)`
_ (foo) // expands to `(a) => a(foo)`
```

It also has the ability to transform a method into a function; When Scala can infer that we need a function we can omit the `_` i.e when passing it to another method.

```scala
scala> def method[A](a: A): A = a
method: [A](a: A)A

scala> method[Int] _
res6: Int => Int = $$Lambda$1094/294649085@53f4c1e6
```

#### Multiple parameter lists
Methods in Scala can actually have multiple parameter lists. Such methods work just like normal methods, except we must bracket each parameter list separately.

Multiple parameter lists have two relevant uses:
1. they look nicer when defining functions inline and they assist with type inference. `fold ( 0 ){ (total, elt) => total + elt }` vs `fold ( 0 ){ (total, elt) => total + elt }`
2. More important is the use of multiple parameter lists to ease type inference. In the example of `def fold[B](id: B)(op: (A, B) => B): B` if the compiler an infer the type of the first parameter list (easy) it now knows the type for the second parameter.

## Generic Optional Values
Many expressions may sometimes produce a value and sometimes not. For example, when we look up an element in a hash table (associative array) by a key, a web request, that service may be down and not reply to us, ... There are a number of ways to model this situation of an optional value. **We could throw an exception, or we could return null when a value is not available. The disadvantage of both these methods is they don’t encode any information in the type system.** => By including this into the type system, you can transform runtime failures to compile-time.

We generally want to write robust programs, and in Scala we try to utilise the type system to encode properties we want our programs to maintain. One common property is “correctly handle errors”. If we can encode an optional value in the type system, the compiler will force us to consider the case where a value is not available, thus increasing the robustness of our code.

#### Question: Generics vs Traits

**Answer: ** Ultimately the decision is up to us. Different teams will adopt different programming styles. However, we look at the properties of each approach to inform our choices:
Inheritance-based approaches—traits and classes—allow us to create permanent data structures with specific types and names. We can name every field and method and implement use-case-specific code in each class. Inheritance is therefore better suited to modelling significant aspects of our programs that are re-used in many areas of our codebase.
Generic data structures—Tuples, Options, Eithers, and so on—are extremely broad and general purpose. There are a wide range of predefined classes in the Scala standard library that we can use to quickly model relationships between data in our code. These classes are therefore better suited to quick, one-off pieces of data manipulation where defining our own types would introduce unnecessary verbosity to our codebase.

## Sequencing Computation

**Map** is an operation that transforms a type F[A] with a function A => B to a result F[B].

**FlatMap** is an operation that transforms a type F[A] with a function A => F[B] to a result F[B].

**Functor** is a type like F[A] that has a map method

**Monad** is a Functor that also has a flatmap method.

**There is a little bit more to being a functor or monad. For a monad we require a constructor, typically called point, and there are some algebraic laws that our map and flatMap operations must obey.**

The general idea is a monad represents a value in some context. The context depends on the monad we’re using. We’ve seen examples where the context is:

* an optional value, such as we might get when retrieving a value from a database;
* an sum of values, which might represent a error message and a value we’re computing with;
* a list of values.

We use `map` when we want to transform the value within the context to a new value, while keeping the context the same. We use `flatMap` when we want to transform the value and provide a new context.
