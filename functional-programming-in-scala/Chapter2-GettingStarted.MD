# Chapter 2 - Getting Started

 * Monomorphic functions are functions that only operate on one type. i.e. `def abs(a: Int): Int` only operates on the type Int.
 * Polymorphic functions (AKA generic functions) are functions that operate on any type, i.e. `def findFirst[T](as: Array[T], f: T => Boolean): Int`. T is what we call a type variable, and it is passed in by a type parameter list. Read more [Polymorphic Functions](PolymorphicFunctions.MD)

* Function literals or anonymous functions in Scala such as (x: A, y: A) => x == y is just syntatic sugar for trait Function2[A,A, Boolean] (AKA (A,A) => Boolean) with a method `apply(a1: A, a2: A)`. They're also known as Lambdas.


 ## Variance

* Scala borrows its concepts of subtyping from Category theory.
      => Category where the objects or types and arrows or functions between types (??? to be clarified)
          -> Subtype relationships
      => Functors are type constructors that take a type and return another type for example `List[_]` with parameter `Int` returns `List[Int]`
          -> Type constructors are covariant if it maintains the direction of the arrows between objects (maintain the direction of subtype relationships).

### Example Covariance / Contravariance
```scala
class Entertainment
class Music extends Entertainment
class Metal extends Music

//this type constructor is said to be Covariant because
//Producer[Metal] <: Producer[Music] <: Producer[Entertainment]
//=> A producer of metal also produces Music and Entertainment and thus Producers[-X] is said to be Covariant
//because it does maintain the direction the arrows of the type parameter.
trait Producer[+X] {
  //==> When X is Entertainment it can produce Metal
  //We call this Covariant because it maintains the order of subtyping
  //Metal <: Music <: Entertainment
  def produce(): X
}

//this type constructor is said to be Contravariant because
//Consumer[Entertainment] <: Consumer[Music] <: Consumer[Metal]
//=> A consumer of entertainment can also consume music and metal and thus Consumer[-X] is said to be Contravariant.
//because it does not maintains the direction the arrows of the type parameter.
trait Consumer[-X] {
  //==> When X is Entertainment it can consume Metal
  def consume(x: X): Unit
}
```
![Contravariance vs Covariance](https://github.com/nomisRev/FunctionalProgrammingInScala/blob/master/Covariance:Contravariance.png)

* A type can be covariant when it does not call methods on the type that it is generic over. If the type needs to call methods on generic objects that are passed into it, it cannot be covariant.
* A type can be contravariant when it does call methods on the type that it is generic over. If the type needs to return values of the type it is generic over, it cannot be contravariant.

```scala
//It's contravariant over it's first parameter and covariant over it's second
//We call this a BiFunctor in Category theory
trait Function1[-T1,+R] {
    def apply(t1: T1): R
}
```
* Function1[Animal, Double] <: Function1[Cat, Double] <-- T1 reverts the subtype relationship
* Function1[Double, Cat] <: Function[Double, Animal] <-- R maintains the subtype relationship

## Ad-hoc polymorphism (Type-class pattern)
 * Ad-hoc polymorphism specifies functionality in an abstract way, and provide a means for different types to choose to implement that functionality and then for other types to require that functionality to be implemented in order to be used. i.e. `Foldable[T]``

 ```scala
 trait Show[A] {
   def format(a: A): String
 }

object ShowInstances {
  implicit val stringShowable = new Show[String] {
    def format(a: String) = "show: " ++ a.toString
  }
  implicit val intShowable = new Show[Int] {
    def format(a: Int) = "show: " ++ a.toString
  }
}
```

Interfaces to type classes are generic methods that accept instances of the type class as implicit parameters. There are two common ways of specifying an interface: Interface Objects and Interface Syntax.

### Interface objects
```scala
object Printer {
    def prettify[A](input: A)
      (implicit printer: Show[A]): String = {
        printer.format(input)
    }
}

import ShowInstances._
Printer.prettify("printed string") //show: printed string
Printer.prettify(42) //show: 42
```

### Interface Syntax
```scala
object ShowSyntax {
    implicit class ShowOps[A](value: A) {
      def prettyPrint(implicit printer: Show[A]): String = {
        printer.format(value)
      }
    }
}

import ShowInstances._
import ShowSyntax._

"printed string".prettyPrint() //show: printed string
42.prettyPrint() //show: 42
```
