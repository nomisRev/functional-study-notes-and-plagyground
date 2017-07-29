# Variance
If we have some type Foo[A], and A is a subtype of B, is Foo[A] a subtype of Foo[B]?
The answer depends on the variance of the type Foo.
The variance of a generic type determines how its supertype/subtype relationships change with respect with its type parameters

###### Invariant

A type `Foo[T]` is invariant in terms of `T`, meaning that the types `Foo[A]` and `Foo[B]` are unrelated regardless of the relationship between `A` and `B`.
This is the default variance of any generic type in Scala.

###### Covariant

A type `Foo[+T]` is **covariant** in terms of `T`, meaning that `Foo[A]` is a supertype of `Foo[B]` if `A` is a supertype of `B`.
Most Scala collection classes are covariant in terms of their contents. **Memory aid: +, positive respects the relationships of the type parameters**

##### Contravariant

A type `Foo[-T]` is **contravariant** in terms of `T`, meaning that `Foo[A]` is a subtype of `Foo[B]` if `A` is a supertype of `B`.
The only example of contravariance that I am aware of is function arguments. **Memory aid: -, negative negates/reverses the relationships of the type parameters**

## Function types

**Functions are contravariant in terms of their arguments and covariant in terms of their return type.** To understand this let's look what kinda of functions we can pass to `map`.

```scala
class Box[A](value: A) {
  /** Apply `func` to `value`, returning a `Box` of the result. */
  def map[B](func: Function1[A, B]): Box[B] =
    Box(func(a))
}
```

* A function from A to B is clearly ok.
* A function from A to a subtype of B is ok because it’s result type will have all the properties of B that we might depend on. This indicates that functions are covariant in their result type.
* A function expecting a supertype of A is also ok, because the A we have in the Box will have all the properties that the function expects.
* A function expecting a subtype of A is not ok, because our value may in reality be a different subtype of A.

## Covariant Sum Types

Let's take a look at our `Maybe` sum type. If we make Maybe covariant in `A` we can now define Empty as an object of type Maybe[Nothing] since Nothing is a subtype of A, Maybe[Nothing] is a subtype of Maybe[A].

```scala
sealed trait Maybe[+A]
final case class Full[A](value: A) extends Maybe[A]
case object Empty extends Maybe[Nothing]
```

This pattern is the most commonly used one with generic sum types. We should only use covariant types where the container type is immutable. If the container allows mutation we should only use invariant types (and you should never do mutation :D).

## Contravariant position

```scala
case class Box[+A](value: A) {
  def set(a: A): Box[A] = Box(a)
}

error: covariant type A occurs in contravariant position in type A of value a
  def set(a: A): Box[A] = Box(a)
          ^
```

Remember that functions, and hence methods, which are just like functions, are contravariant in their input parameters. In this case we have specified that A is covariant but in set we have a parameter of type A and the type rules requires A to be contravariant here. This is what the compiler means by a “contravariant position”.

The solution is introduce a new type that is a supertype of A. We can do this with the notation [AA >: A] like so:
```scala
case class Box[+A](value: A) {
  def set[AA >: A](a: AA): Box[AA] = Box(a)
}
```

This is the same for function parameters like flatMap. The function `f` is a parameter, and thus in a contravariant position. This means we accept supertypes of `f`. It is declared with type `B => Sum[A, C]` and thus a supertype is covariant in `B` and contravariant in `A` and `C`. `B` is declared as covariant, so that is fine. `C` is invariant, so that is fine as well. `A` on the other hand is covariant but in a contravariant position. Thus we have to apply the same solution we did for Box above.

```scala
sealed trait Sum[+A, +B] {
  def flatMap[AA >: A, C](f: B => Sum[AA, C]): Sum[AA, C] =
    this match {
      case Failure(v) => Failure(v)
      case Success(v) => f(v)
    }
}
final case class Failure[A](value: A) extends Sum[A, Nothing]
final case class Success[B](value: B) extends Sum[Nothing, B]

```

## Type bounds

We have see some type bounds above, in the contravariant position pattern.
Type bounds extend to specify subtypes as well as supertypes.
The syntax is `A <: Type` to declare `A` must be a subtype of `Type` and `A >: Type` to declare a supertype.
