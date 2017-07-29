package red_book_exercises.chapter_3_functional_data_structures

//import exercises.chapter_3_functional_data_structures.{ List => List2, Nil => Nil2}

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  /**
    * foldLeft(Cons(1, Cons(2, Nil)), Cons(3, Cons(4,Nil)), Nil)((a,b) => Cons(b,a))
    * foldLeft(Cons(2, Nil),Cons(1, Cons(3, Cons(4,Nil))), Nil)((a,b) => Cons(b,a))
    * foldLeft(Nil, Cons(2, Cons(1, Cons(3, Cons(4,Nil)))), Nil)((a,b) => Cons(b,a))
    * Cons(2, Cons(1, Cons(3, Cons(4,Nil))))
    *
    * foldRight(Cons(1, Cons(2, Nil)), Cons(3, Cons(4,Nil))) ((a,b) => Cons(b,a))
    * Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil) ((a,b) => Cons(b,a))
    * Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil) ((a,b) => Cons(b,a))
    * Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil) ((a,b) => Cons(b,a))
    * Cons(1, Cons(2, Cons(3, (Nil))))
    *
    */
  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((a, b) => Cons(a, b))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)((x, y) => x + y)

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def product3(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => if (n <= 0) l else drop(t, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  /**
    * Note that we're copying the entire list up until the last element.
    * Besides being inefficient, the natural recursive solution will use a stack frame for each element of the list,
    * which can lead to stack overflows for large lists (can you see why?).
    * With lists, it's common to use a temporary, mutable buffer internal to the function (with lazy lists or streams,
    * which we discuss in chapter 5, we don't normally do this). So long as the buffer is allocated internal to the function, the mutation is not observable and RT is preserved.
    * Another common convention is to accumulate the output list in reverse order, then reverse it at the end, which doesn't require even local mutation.
    * We'll write a reverse function later in this chapter.
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => Nil
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }

    go(l)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => b + 1)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((b, _) => b + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def mapInts(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def mapDouble(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((h, t) => Cons(h.toString(), t))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def map_2[A, B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]

    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => buf += f(h); go(t)
    }

    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  def filter[A](l: List[A])(f: A => Boolean) = foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]

    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => if (f(h)) buf += h; go(t)
    }

    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  def filter_3[A](l: List[A])(f: A => Boolean) = flatMap(l)(a => if (f(a)) List(a) else Nil)

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }

  /**
    * foldLeft(Cons(1, Cons(2, Cons(3, Nil))),Nil)((a,b) => Cons(b,a))
    * foldLeft(Cons(2, Cons(3, Nil)),Cons(1, Nil))((a,b) => Cons(b,a))
    * foldLeft(Cons(3,Nil), Cons(2, Cons(1,Nil) ))((a,b) => Cons(b,a))
    * foldLeft(Nil, Cons(3, Cons(2, Cons(1,Nil) ) ))((a,b) => Cons(b,a))
    *
    * Cons(3, Cons(2, Cons(1,Nil) ) )
    */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b,a) => Cons(a, b))
}
