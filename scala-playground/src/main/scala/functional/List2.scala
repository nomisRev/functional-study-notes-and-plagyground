package functional


trait List2[A] {

  def fold[B: Monoid](f: A => B): B = this match {
    case Nil2 => implicitly[Monoid[B]].id
    case Cons2(h, t) => implicitly[Monoid[B]].op(f(h), t.fold(f))
  }
  

  // Define the usual List API.
  //
  // * Constructors (on the object)
  // nil
  // cons
  // * methods
  // map
  // flatMap
  // foldLeft
  // filter
  // reverse
  // append
  // any other useful functions
}

case class Cons2[A](head: A, tail: List2[A]) extends List2[A]
case object Nil2 extends List2[Nothing]