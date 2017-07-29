# Essential Scala - Chapter 3 Objects and Classes

## Classes

A class is a template for creating objects that have similar methods and fields. In Scala a class also defines a type, and objects created from a class all share the same type

Each call to new creates a distinct object of the same type.

```Scala
class Person {
  val firstName = "Noel"
  val lastName = "Welsh"
  def name = firstName + " " + lastName
}
```

Scala classes are all subclasses of java.lang.Object and are, for the most part, usable from Java as well as Scala. The default printing behaviour of Person comes from the toString method defined in java.lang.Object .

Constructor arguments and fields are often redundant.

```Scala
class Person ( val firstName: String, val lastName: String) {
  def name = firstName + " " + lastName
}
```

**All Scala methods and constructors support keyword parameters and default parameter values.**

```scala
val person = new Person(lastName = "Last", firstName = "first")
def greet(firstName: String = "Some", lastName: String = "Body") =
  "Greetings, " + firstName + " " + lastName + "!"
```

### Scala's Type Hierarchy

Scala has a grand supertype called `Any`, under which there are two types, `AnyVal` and `AnyRef`. `AnyVal` is the supertype of all value types, which `AnyRef` is the supertype of all “reference types” or classes. All Scala and Java classes are subtypes of `AnyRef`.

There are two special types at the bottom of the hierarchy. `Nothing` is the type of throw expressions, and `Null` is the type of the value null.

```Scala
def badness = throw new Exception( "Error" )
// badness: Nothing

null
// res: Null = null

val bar = if ( true ) 123 else badness
// bar: Int = 123

val baz = if ( false ) "it worked" else null
// baz: String = null
```

Although the types of badness and res are Nothing and Null respectively, the types of bar and baz are still sensible. This is because Int is the least common supertype of Int and Nothing , and String is the least common supertype of String and Null .

### Objects as Functions
Lets take a Counter as an example.

```Scala
class Counter(val i: Int) {
  def inc = new Counter(i + 1)
  def dec = new Counter(i - 1)
}
```

Let's say we want to increment with 5. Let's implement a class `Adder`

```Scala
class Adder(amount: Int) {
  def add(in: Int) = in + amount
}
```

Let's now enable Counter to work with Adder

```Scala
class Counter(val i: Int) {
  def inc = new Counter(i + 1)
  def dec = new Counter(i - 1)
  def adjust(adder: Adder) = new Counter(adder.add(i))
}
```

We described an `Adder` as an object representing a computation - a bit like having a method that we can pass around as a value. This is such a powerful concept that Scala has a fully blown set of language features for creating objects that behave like computations. These objects are called functions, and are the basis of *functional programming*.

#### The apply method
In Scala, by convention, an object can be "called" like a function if it has a method called `apply`. Naming a method `apply` affords us a special shortened call syntax: `foo.apply(args)` => `foo(args)`

With function application syntax, we now have first class values that behave like computations. Unlike methods, objects can be passed around as data. This takes us one step closer towards true functional programming in Scala.

## Companion objects

Sometimes we want to create a method that logically belongs to a class but is independent of any particular object. One common use case is auxiliary constructors.

Scala programmers almost always prefer to implement additional constructors as apply methods on an object with the same name as the class. We refer to the object as the companion object of the class.

```Scala
class Timestamp( val seconds: Long)

object Timestamp {
  def apply (hours: Int, minutes: Int, seconds: Int): Timestamp =
    new Timestamp(hours* 60 * 60 + minutes* 60 + seconds)
}

Timestamp( 1 , 1 , 1 ). seconds
// res: Long = 3661
```

As we saw earlier, Scala has two namespaces: a space of type names and a space of value names . This separation allows us to name our class and companion object the same thing without conflict.

## Case classes
Case classes are an exceptionally useful shorthand for defining a class, a companion object, and a lot of sensible defaults in one go.

```Scala
case class Person (firstName: String, lastName: String) {
  def name = firstName + " " + lastName
}

val dave = new Person ( "Dave" , "Gurnell" ) // we have a class
// dave: Person = Person(Dave,Gurnell)

Person // and a companion object too
// res: Person.type = Person
```

* A field for each constructor argument —we don’t even need to write val in our constructor definition, although there’s no harm in doing so.
* A default toString method that prints a sensible constructor-like representation of the class
* Sensible equals , and hashCode methods that operate on the field values in the object.
* A copy method that creates a new object with the same field values as the current one
* The companion object also contains code to implement an extractor pattern for use in pattern matching.

### Case objects
If you ever find yourself defining a case class with no argument you can instead define a *case object*. The differences between a case object and a regular object are:
* The `case object` keyword defines a class and an object, and makes the object an (the only) instance of the class.
* With a case object we still get all of the functionality defined for case classes.

### Value and reference equality
Scala's `==` operator is different from Java's - it delegates to `equals` rather than comparing values on reference identity. Scala has an operator called `eq` to check reference identity but that is rarely used in application code.

## Pattern matching

Pattern matching is like an extended if expression that allows us to evaluate an expression depending on **the “shape” of the data**.

```Scala
case class Person (firstName: String, lastName: String)

object Stormtrooper {
  def inspect (person: Person): String = person match {
    case Person ( "Luke" , "Skywalker" ) => "Stop, rebel scum!"
    case Person ( "Han" , "Solo" ) => "Stop, rebel scum!"
    case Person (first, last) => s "Move along, $first"
  }
}
```
#### Pattern matching syntax
```scala
expr0 match {
  case pattern1 => expr1
  case pattern2 => expr2
}
```

* expr0 evaluates to the value we match
* The patterns, or guards, `pattern1`, `pattern2`, and so on are checked against this value in order
* The right-hand side expressions (`expr1`, `expr2`, and so on) of the first pattern that matches is evaluated.

For case classes the pattern syntax matches the constructor syntax. `Person("Luke", "Skywalker")`. A pattern to match against the `Person` type is written `Person(pat0, pat1)` where `pat0` and `pat1` are patterns to match against the `firstName` and `lastName` respectively. (patX is called componentX in Kotlin)

There are four possible patterns we could use in place of `pat0` and `pat1`:
1. A name, which matches any value at that position and binds it to the given name. `Person(first, last)` binds the name `first` to `"Luke"` and `last` to `"Skywalker"`.
2. An underscore (_), which matches any value and ignores it. `Person(first, _)`
3. A literal, which successfully matches only the value the literal represents. `Person("Han","Solo")` will only match with first name "Han" and last name "Solo"
4. Another case class using the same constructor style syntax.

** There is a lot more we can do with pattern matching TBD later **
