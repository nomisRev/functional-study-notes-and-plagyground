package functional

sealed trait NonEmptyList[A] {

  def fold[B](f: A => B)(implicit F: Semigroup[B]): B = this match {
    case Single(v) => f(v)
    case NECons(h, r) => F.op(f(h), r.fold(f))
  }

  def foldLeft[B](f: A => B)(implicit F: Semigroup[B]): B = this match {
    case Single(v) => f(v)
    case NECons(h, r) => F.op(r.fold(f), f(h))
  }

  def map[B](f: A => B): NonEmptyList[B] = this match {
    case Single(v) => Single(f(v))
    case NECons(h, t) => NECons(f(h), t.map(f))
  }

  def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] = this match {
    case Single(v) => f(v)
    case NECons(h, t) => f(h).append(t.flatMap(f))
  }

  def append(l: NonEmptyList[A]): NonEmptyList[A] = this match {
    case Single(v) => NECons(v, l)
    case NECons(h, t) => NECons(h, t.append(l))
  }

  def append(a: A): NonEmptyList[A] = this match {
    case Single(v) => NECons(v, Single(a))
    case NECons(h, t) => NECons(h, t.append(a))
  }

  def reverse(): NonEmptyList[A] = this match {
    case s@Single(_) => s
    case NECons(h,t) => t.reverse().append(h)
  }

  // Define a NonEmptyList API.
  //
  // * Constructors (on the object)
  // single
  // cons
  // * methods
  // map
  // flatMap
  // foldLeft
  // reverse
  // append
  // any other useful functions


}


final case class Single[A](value: A) extends NonEmptyList[A]
final case class NECons[A](head: A, tail: NonEmptyList[A]) extends NonEmptyList[A]