# Chapter 3 - Functional Data Structures

* A functional data structure is operated on using only pure functions. A pure function must not change data in place or perform other side effects.
Therefore, functional data structures are by definition immutable.

## Data sharing
![Data sharing](https://github.com/nomisRev/FunctionalProgrammingInScala/blob/master/DataSharing.png)

When we add an element 1 to the front of an existing list, say xs, we return a new list, in this case `Cons(1,xs)`. Since lists are immutable, we don’t need to actually copy xs ; we can just reuse it. This is called data sharing.

* We say that functional data structures are persistent, meaning that existing references are never changed by operations on the data structure.

## Generalizing higher-order functions

In FP it's common practice to pass generalize functions by pulling away subexpressions out into function arguments.

For example given following two functions, the differences are the type Int/Double, its short circuiting/initial value and the operation to combine the results. We can extract both into a type parameter, a value and function argument. The resulting function is foldRight.
```scala
def sum(ints: List[Int]): Int = ints match {
  case Nil => 0
  case Cons(x,xs) => x + sum(xs)
}

def product(ds: List[Double]): Double = ds match {
  case Nil => 1.0
  case Cons(x, xs) => x * product(xs)
}

def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B) : B = as match {
  case Nil => z
  case Cons(x,xs) => f(x, foldRight(xs,z)(f))
}
```

Let's take a look at a print trace of `foldRight(List(1,2,3),0)(_+_)`
```scala
foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)(_+_)
(1 + foldRight(Cons(2, Cons(3, Nil)), 0)(_+_)
(1 + (2 + foldRight(Cons(3, Nil), 0)(_+_)
(1 + (2 + (3 + foldRight(Nil, 0)(_+_)
(1 + (2 + (3 + (0))))
6
```
Note that foldRight must traverse all the way to the end of the list (pushing frames onto the call stack as it goes) before it can begin collapsing it. And thus is *NOT* stack-safe.

So let's take a look at `foldLeft` which is stack-safe since it can be expressed as a tailrec function and what the difference is with `foldRight`.

```scala
@annotation.tailrec
def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
  case Nil => z
  case Cons(h,t) => foldLeft(t, f(z,h))(f)
}
```

Let's take a look at a print trace of `foldLeft(List(1,2,3),0)(_+_)`
```scala
foldLeft(Cons(1, Cons(2, Cons(3, Nil))), 0)(_+_)
foldLeft(Cons(2, Cons(3, Nil)), (0 + 1))(_+_)
foldLeft(Cons(3, Nil), ((0 + 1) + 2))(_+_)
foldLeft(Nil, (((0 + 1) + 2) + 3))(_+_)
(((0 + 1) + 2) + 3)
6
```

As you can immediately see when comparing the print traces foldRight associates to the right (elements will be accumulated in right-to-left order) and foldLeft associates to the left (elements will be accumulated in left-to-right order). This means that when your function f is associative it does not matter which fold implementation you use *although foldRight is implemented in means of foldLeft in Scala to be stack-safe and thus foldLeft is faster.*


###### Example function with non-associative property
```scala
foldRight(List(10.0,2.0),1.0)(_/_)
foldRight(Cons(10.0, Cons(2.0, Nil)),1.0)(_/_)
(10.0 / foldRight(Cons(2.0, Nil),1.0)(_/_))
(10.0 / (2.0 /foldRight(Nil,1.0)(_/_)))
(10.0 / (2.0 / 1.0))
5

foldLeft(List(10.0,2.0),1.0)(_/_)
foldLeft(Cons(10.0, Cons(2.0, Nil)),1.0)(_/_)
foldLeft(Cons(2.0, Nil),(10.0 / 1.0))(_/_)
foldLeft(Nil, ((10.0 / 1.0) / 2.0))(_/_)
((10.0 / 1.0) / 2.0)
0.05
```

##### Algebraic data types (ADTs) and encapsulation

One might object that algebraic data types violate encapsulation by making public the internal representation of a type. In FP, we approach concerns about encapsulation differently—we don’t typically have delicate mutable state which could lead to bugs or violation of invariants if exposed publicly. Exposing the data constructors of a type is often fine, and the decision to do so is approached much like any other decision about what the public API of a data type should be.

##### Scala type inference

With following function in Scala `def dropWhile[A](l: List[A], f: A => Boolean): List[A]` when we pass it an anonymous function for parameter f, we have to specify the type of its argument `dropWhile(xs, (x: Int) => x < 4)`. It’s a little unfortunate that we need to state that the type of x is Int. Scala can infer this fact if we group dropWhile into two argument lists `def dropWhile[A](as: List[A])(f: A => Boolean): List[A]`, the function can now be called `dropWhile(xs)(x => x < 4)`.

What we actually did here was curry the function, `(List[A], A => Boolean) => List[A]` to `List[A] => (A => Boolean) => List[A]`.

* When a function definition contains multiple argument groups, type information flows from left to right across these argument groups. (This is an unfortunate restriction of the Scala compiler; other functional languages like Haskell, Kotlin and OCaml provide complete type inference)

##### Scala underscore notation for anonymous functions

The anonymous function (x,y) => x + y can be written as _ + _ in situations where the types of x and y could be inferred by Scala. Each underscore in an anonymous function expression like _ + _ introduces a new (unnamed) function parameter and references it.

Examples:
* _ + _ == (x,y) => x + y
* _ * 2 ==  x => x * 2
* _.head == xs => xs.head
* _ drop _ == (xs,n) => xs.drop(n)
* _ :: _ == (xs,ys) => xs.append(ys)

**Use this syntax judiciously. The meaning of this syntax in expressions like `foo(_, g(List(_ + 1), _))` can be unclear.**
