# Essential Scala - Chapter 4 Modeling data with Traits

We’re going to look at modelling data and learn a process for expressing in Scala any data model defined in terms of logical `ors` and `ands`. Using the terminology of object-oriented programming, we will express `is-a` and `has-a` relationships. In the terminology of functional programming we are learning about `sum` and `product` types, which are together called algebraic data types.

## The product type pattern

The product type patterns models data that contains other data. We might describe this as `A has a B AND C`. For example, a Cat has a colour AND a favourite food.

**If A has a b (with type B ) and a c (with type C ) write**
```Scala
case class A(b: B, c: C)

OR

trait A {
  def b: B
  def c: C
}
```

## The sum type pattern

The sum type pattern models data that is two or more distinct cases. We might describe this as `A is a B or C`. For example, a Feline is a Cat , Lion , or Tiger.

**If A is a B or C write**
```scala
sealed trait A
final case class B () extends A
final case class C () extends A
```

## Missing patterns
We have covered the `is-a or` with the sum pattern and the `has-a and` with the product pattern. But what about `is-a and` and `has-a or`?

The `is-a and` pattern means that `A is a B and C`. Somewhat the inverse of the `is-a or`/sum pattern.

```scala
trait B
trait C
trait A extends B with C
```

If we want to represent that some data conforms to a number of different interfaces we will often be better off using a type class. Some legitimate use cases are:
* for modularity, using the [cake pattern](http://www.cakesolutions.net/teamblogs/2011/12/19/cake-pattern-in-depth)(and its references).
* sharing implementation across several classes where it doesn’t make sense to make default implementations in the main trait.

The `has-a or` patterns means that `A has a B or C`. There are two ways to implement this: `A has a d of type D, where D is a B or C.` and we can implement this by combining the sum and product pattern.

```scala
trait A {
  def d: D
}

sealed trait D
final case class B() extends D
final case class C() extends D
```

OR we could implement this as `A is a D or E, and D has a B and E has a C`.

```scala
sealed trait A
final case class D (b: B) extends A
final case class E (c: C) extends A
```

##### Algebraic data type
An algebraic data type is any data that uses the above two patterns. In the functional programming literature, data using the `has-a and` pattern is known as a product type, and the `is-a or` pattern is a sum type (or, more succinctly, the product and sum types).

# Working with data

We'll see two variants of the *Structural recursion* pattern, **polymorphism** and **pattern matching**

Structural recursion is the precise opposite of the process of building an algebraic data type. If `A` has a `B` and a `C` (the product type patter), to construct an `A` we need a `B` and a `C`. The sum -and product type pattern tell us how to combine data to make bigger data. Structural recursion is essentially the process of breaking down data into smaller pieces.

## Structural recursion using polymorphism
Polymorphic dispatch or polymorphism is a fundamental object-oriented technique.

If we define a method in a trait, and have different implementations in classes extending that trait, when we call that method the actual implementation on the actual concrete instance will be used.

##### The product type polymorphism pattern
If `A` has a `b` (with type `B` ) and a `c` (with type `C` ), and we want to write a method `f` returning an `F`

```scala
case class A(b: B, c: C) {
  def f: F = ???
}
```

##### The sum type polymorphism pattern
If `A` is a `B` or `C`, and we want to write a method `f` returning an `F` , define `f` as an abstract method on `A` and provide concrete implementations in `B` and `C`.

```scala
sealed trait A {
  def f: F
}

final case class B() extends A {
  def f: F = ???
}

final case class C() extends A {
  def f: F = ???
}
```

## Structural recursion using pattern matching
Structural recursion with pattern matching proceeds along the same lines as polymorphism. We simply have a case for every subtype, and each pattern matching case must extract the fields we’re interested in.

##### The product type pattern matching pattern
If `A` has a `b` (with type `B`) and a `c` (with type `C`), and we want to write a method `f` that accepts an `A` and returns an `F`.

``` scala
def f(a: A) = a match {
  case A(b,c) => ???
}
```

##### The sum type pattern matching pattern
If `A` is a `B` or `C`, and we want to write a method `f` accepting an `A` and returning an `F`, define a pattern matching case for `B` and `C`.

```scala
def f(a: A) = a match {
  case B() => ???
  case C() => ???
}
```

## A concrete example

We have to implement a way to resolve what kind of food a Feline eats. Let's see how this looks using polymorphism (OOP) vs pattern matching (FP).

```scala
sealed trait Feline
final case class Lion () extends Feline                           // Eats antelope
final case class Tiger () extends Feline                          // Eats tigerFood
final case class Panther () extends Feline                        // Eats licorice
final case class Cat (favouriteFood: String) extends Feline       // Eats favouriteFood
```
Let's model our food in a good manner like we've seen before. Food is antelope or tigerFood or licorice or a cats favouriteFood.

```scala
sealed trait Food
final case object Antelope extends Food
final case object TigerFood extends Food
final case object Licorice extends Food
final case class CatFood (food: String) extends Food
```

In order to do this with polymorphism we have to **change our existing implementation of `Feline`**.

```scala
sealed trait Feline {
  def dinner: Food
}
final case class Lion () extends Feline {
  def dinner: Food = Antelope
}
final case class Tiger () extends Feline {
  def dinner: Food = TigerFood
}
final case class Panther () extends Feline {
  def dinner: Food = Licorice
}
final case class Cat (favoriteFood: String) extends Feline {
  def dinner: Food = CatFood(favoriteFood)
}
```

When using pattern matching we have 2 options, define the method in our existing trait `Feline` or in another object. Let's see both.

```
sealed trait Feline {
  def dinner: Food = this match {
    case Lion() => Antelope
    case Tiger() => TigerFood
    case Panther() => Licorice
    case Cat(food) => CatFood(food)
  }
}

object Dinner {
  def dinner(feline: Feline) = feline match {
    case Lion() => Antelope
    case Tiger() => TigerFood
    case Panther() => Licorice
    case Cat(food) => CatFood(food)
  }
}
```

This is the main point we want to make with structural recursion: the code follows the shape of the data, and can be produced in an almost mechanical way.

#### Choosing an implementation
The first two methods give the same result. The method is available on the type `Feline`. Use whichever is more convenient. Using this method we can only have one implementation, when we implement the method in an external object we can provide multiple implementations, for example Breakfast, Lunch and Dinner.

Rule of thumb. If a method only depends on other fields and methods in a class and you only need one implementation it is a good candidate to be implemented within that class. Otherwise if it depends on other data (i.e. `Cook`) consider implementing it using pattern matching outside of the class in question.

#### object-oriented vs functional extensibility
In classic functional programming we have no objects, only data without methods and functions. We can mimic it in Scala using the algebraic data type pattern and pattern matching in methods defined on external objects.

Classic object oriented style uses polymorphism and allow open extension of classes. In Scala terms this means no sealed traits.

With OO style we can easily add new data, by extending a trait, but adding a new method requires us to change existing code. For example, adding `SnowLeopard` can easily be done by `class SnowLeopard extends Feline`, but adding a method `run` to the trait `Feline` requires us to modify all existing implementations of `Feline`.

With functional style we can easily add a new method but adding new data requires us to modify existing code. For example, adding the method `run` is easy because we just use pattern matching to exhaust all cases and define the implementation of `run` but adding a `SnowLeopard` requires us to modify all existing methods for `Feline`. (If your data is modeled correctly you have compile checks for that though ;) )

|    | Add new method          | Add new data            |
| -- |:-----------------------:| -----------------------:|
| OO | Change existing code    | Existing code unchanged |
| FP | Existing code unchanged | Change existing code    |

**Type-classes can be used to achieve OO-style extensibility.**

## Recursive data

A particular use of algebraic data types that comes up very often is defining recursive data . This is data that is defined in terms of itself, and allows us to create data of potentially unbounded size (though any concrete instance will be finite).

We can't define recursive data like `final case class Broken(broken: Broken)` as we could never instantiate such a type - the recursion never ends. So we must define a distinction between the **base case**, which is the case that ends the recursion and the **recursive case**.

```Scala
sealed trait IntList
final case object End extends IntList
final case Pair(head: Int, tail: IntList) extends IntList

Pair(1, Pair(2, Pair(3, End)))    // a list containing 1, 2 and 3
```

**This data structure is known as a singly-linked list**

We can apply the same structural recursion patterns to process a recursive algebraic data type. The only wrinkle is that we must make a recursive call when the data definition is recursion.

```Scala
def sum(i: IntList): Int = i match {
  case End => 0
  case Pair(h, t) => h + sum(t)
}
```

### Understanding the base case and the recursive case

Our patterns will carry us most of the way to a correct answer, but we still need to supply the method bodies for the base and recursive cases. Some guidance:
* For the base case we should generally return the *identity* for the function we’re trying to compute. The identity is an element that doesn’t change the result. i.e. 0 is the identity for addition.
* For the recursive case, assume the recursion will return the correct result and work out what you need to add to get the correct answer. We saw this for sum, where we assume the recursive call will give us the correct result for the tail of the list and we then just add on the head.

###### Tail recursion

You may be concerned that recursive calls will consume excessive stack space and doing so for large recursive data will result in an stack overflow. Scala can apply an optimisation, called tail recursion.

A tail call is a method call where the caller immediately returns the value.

```Scala
def method1: Int = 1
def tailCall: Int = method1
def notATailCall: Int = method1 + 2
```

So let's rewrite out previous sum method:

```Scala
def sum(i: IntList, total: Int = 0) = i match {
  case End => total
  case Pair(h,t) => sum(t, total + h)
}
```

This transforms stack allocation into heap allocation, which sometimes is a win, and other times is not.

Normally you should not have to work with tail recursion as there is a rich collections library that covers the most common cases where tail recursion is used.

## Json example

In the compiler literature this is known as an *abstract syntax tree*. This way we can describe our data without worrying about the syntax of the language.

An Json is a JsNumber, JsString, JsBoolean, JsNull, JsSequence or a JsObject
  * With JsSequence being a SeqCell or SeqEnd
  * And JsObject being a JsonCell or ObjectEnd

Json ::= JsNumber value:Double
       | JsString value:String
       | JsBoolean value:Boolean
       | JsNull
       | JsSequence
       | JsObject

JsSequence ::= SeqCell head:Json tail:JsSequence
             | SeqEnd

JsObject ::= ObjectCell key:String value:Json tail:JsObject
           | ObjectEnd

Now we have a clear definition of our data it's trivial to implement.

```Scala
sealed trait Json
final case class JsNumber(value: Double) extends Json
final case class JsString(value: String) extends Json
final case class JsBoolean(value: Boolean) extends Json
final case object JsNull extends Json
sealed trait JsSequence extends Json
final case class SeqCell(head: Json, tail: JsSequence) extends JsSequence
final case object SeqEnd extends JsSequence
sealed trait JsObject extends Json
final case class ObjectCell(key: String, value: Json, tail: JsObject) extends JsObject
final case object ObjectEnd extends JsObject
```
