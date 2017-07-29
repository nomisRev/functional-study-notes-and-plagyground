# Essential Scala - Chapter 4 Modelling Data with Traits

Traits are templates for creating classes, in the same way that classes are templates for creating objects. Traits allow us to express that two or more classes can be considered the same, and thus both implement the same operations.

```Scala
import java.util.Date

trait Visitor {
  def id: String // Unique id assigned to each user
  def createdAt: Date // Date this user first visited the site

  // How long has this visitor been around?
  def age: Long = new Date(). getTime - createdAt. getTime
}

case class Anonymous (id: String, createdAt: Date = new Date()) extends Visitor

case class User ( id: String, email: String, createdAt: Date = new Date() ) extends Visitor
```

## Traits vs Classes
* A trait cannot have a constructor - we can't create objects directly from a trait.
* Traits can define abstract methods that have names and type signatures but no implementation.

## Sealed Traits

In many cases we can enumerate all the possible classes that can extend a trait. Such as a `Anonymous` or `User` as a website Visitor. We can model this with a *sealed trait*, which allows the compiler to provide extra checks for us.

```Scala
sealed trait Visitor {
  def id: String
  def createdAt: Date
  def age: Long = new Date().getTime () - createdAt.getTime()
}
```

When we mark a trait as sealed we **must** define all of its subtypes in the same file.

**Advantages:**
* Once the trait is sealed, the compiler knows the complete set of subtypes and will warn us if a pattern matching expression is missing a case.
* We can control extension points of sealed traits and thus make stronger guarantees about the behavior of subtypes.

**DIFFFERENCE WITH KOTLIN!!!** Since in Scala classes are not final by default, we could extend `User` or `Anonymous` further elsewhere. If we want to prevent this possibility we should declare them as `sealed` (if we want to allow extensions within the file) or `final` if we want to disallow all extensions.

## Data modeling
Good Scala developers don’t just use types to model data. Types are a great way to put artificial limitations in place to ensure we don’t make mistakes in our programs.

Dividing by zero is a tricky problem - it can lead to exceptions.

```Scala
sealed trait DivisionResult
final case class Finite(value: Int) extends DivisionResult
object Infinte extends DivisionResult


object Divide {
  def apply(i1: Int, i2: Int): DivisionResult = {
    if (i2 == 0) Infinte else Finite(i1 / i2)
  }
}

scala> Divide(10,2)
res0: DivisionResult = Finite(5)

scala> Divide(10,0)
res1: DivisionResult = Infinte$@1e253c9d
```
