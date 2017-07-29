# Collections
# Sequences
A sequence is a collection of items with a defined and stable order. Sequences are one of the most common data structures.

```scala
scala> val sequence = Seq(1,2,3)
sequence: Seq[Int] = List(1, 2, 3)
```

This immediately shows off a key feature of Scala's collections, the separation between interfaces and implementation.

Some of the most important operations are show below.

```scala
scala> sequence.apply(0) //res0: Int = 1

scala> sequence(0) //res1: Int = 1

scala> sequence(3) //java.lang.IndexOutOfBoundsException: 3

scala> sequence.head //res3: Int = 1

scala> sequence.tail //res4: Seq[Int] = List(2, 3)

scala> Seq().head //java.util.NoSuchElementException: head of empty list

scala> Seq().headOption // res6: Option[Nothing] = None

scala> sequence.length //res7: Int = 3

scala> sequence.contains(2) //res8: Boolean = true

scala> sequence.find( _ == 3) //res9: Option[Int] = Some(3)

scala> sequence.find( _ >4) //res10: Option[Int] = None

scala> sequence.filter(_ > 1) //res13: Seq[Int] = List(2, 3)

```

## Appending/prepending elements

There are many ways to add elements to a sequence.

```scala
scala> sequence.:+(4) //res15: Seq[Int] = List(1, 2, 3, 4)

scala> sequence :+ 4 //res16: Seq[Int] = List(1, 2, 3, 4)

scala> sequence.+:(0) //res18: Seq[Int] = List(0, 1, 2, 3)

scala> 0 +: sequence //res19: Seq[Int] = List(0, 1, 2, 3)

scala> sequence ++ Seq(4,5,6) //res20: Seq[Int] = List(1, 2, 3, 4, 5, 6)
```

**Something very important is going on here!** The trailing colon in `+:` makes it right associative, so we write the operator-style expression the other way around. This is another of Scala's general syntax rules - any method ending with a `:` character ebcones right associative when written as an infix operator. **Extra information about Scala and the origins of this rule from Math (https://scalaeyeforthejavaguy.blogspot.be/2013/06/infix-and-postfix-and-prefix.html)**

# Lists

The default implementation of Seq is a List, which is a classic linked list data structure similar.

An empty list is represented by the singleton object `Nil`. Longer lists can be created by prepending elements in classic linked-list style using the `::` method, which is equivalent to `+::`.

```scala
val list = 1 :: 2 :: 3 :: Nil //list: List[Int] = List(1,2,3)
4 :: 5 :: list //List(4,5,1,2,3)
```

Finally the `:::` method is a right-associative List-specific version of `++` `List(1,2,3) :: List(4,5,6) = List(1,2,3,4,5,6)`

**:: and ::: are specific to lists whereas +:, :+ and ++ work on any type of sequence**

Lists and Sequence are so commonly used in Scala that they don't need to be imported. Vector and Queue have to be manually imported into scope.

# Exercices
#### Reading documentation
1. There is a synonym of length defined on Seq - what is it called?
  * `length` and `size` yield the same result.

2. There are two methods for retrieving the first item in a List - what are they called and how do they differ?
 * `head` and `headOption`. `head` will throw an exception if the list is empty (losing type support and compile checks) and `headOption` returns Some(_) or None.

3. What method can be used to display the elements of the sequence as a String?
 * `mkString`, `mkString(sep: String)` or `mkString(start: String, sep: String, end: String)`

4. What method of Option can be used to determine whether the option contains a value?
  * `nonEmpty`
