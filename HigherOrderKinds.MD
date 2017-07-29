# Higher-order kinds

**Goal**: reducing code duplication with type constructor polymorphism. See [Polymorphic Functions](PolymorphicFunctions.MD) and [Getting Started](Chapter2-GettingStarted.MD) for extra info on polymorphism.

```
trait Iterable[T] {
  def filter[T](p: T => Boolean): Iterable[T]
  def remove[T](p: T => Boolean): Iterable[T] = filter(x => !p(x))
}
```

We have a trait Iterable which is a type constructor that takes one type parameter T, and it defines two functions filter and remove which both take in the same type parameter and return the type constructed by `Iterable[_]`.
Here remove syntactic sugar which and thus has a default implementation to avoid unnecessary code.

```
trait List[T] extends Iterable[T] {
  def filter[T](p: T => Boolean): List[T]
  override def remove[T](p: T => Boolean): List[T] = filter(x => !p(x))
}
```

When filtering a List[T], we expect to get a List[T] as a result and not an Iterable[T]. Thus List[T] overrides filter to refines its result type covariantly.

**Solution:** To avoid this unnecessary code we abstract over the type constructor[1] that represents the container of the result of `filter` and `remove`.

By doing so we can change the signature of `Iterable`

```
trait Iterable[T, Container[X]] {
  def filter[T](p: T => Boolean): Container[T]
  def remove[T](p: T => Boolean): Container[T] = filter(x => !p(x))
}

trait List[T] extends Iterable[T, List]
```

=> This is called type constructor polymorphism.

So lets take a deeper dive into how to use type constructor polymorphism to factor out boilerplate code without losing type-safety.
Given we want to generalize functions like map, flatmap and filter we need 2 collection specific operations; iterating over a collection and producing a new collection.

```
trait Builder[Container[X], T] {
  def +=(el: T): Unit
  def finalise(): Container[T]
}

trait Iterator[T] {
  def next(): T
  def hasNext(): T
  def foreach(op: T => Unit): Unit = while(hasNext()) op(next())
}
```

[1] A type constructor is a constructor that takes a type parameter and returns a new type. i.e. List[T] takes a parameter T (i.e. Int) and then returns a new type (List[Int]).
