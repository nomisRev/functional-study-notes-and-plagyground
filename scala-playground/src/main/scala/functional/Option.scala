package functional

// Define the usual Option API.
//
// * Constructors (on the object)
// some
// none
// * methods
// map
// flatMap
// orElse
// getOrElse
// any other useful functions
sealed trait Option[+A] {

  @inline def fold[B](n: => B, s: A => B): B = this match {
    case None => n
    case Some(v) => s(v)
  }

  @inline def map[B](f: A => B): Option[B] = fold(None, a => Some(f(a)))

  @inline def flatMap[B](f: A => Option[B]): Option[B] = fold(None, f(_))

  @inline def orElse[B >: A](f: => Option[B]): Option[B] = this match {
    case None => f
    case s@Some(_) => s
  }

  @inline def getOrElse[B >: A](f: => B): B = fold(f, a => a)

  @inline def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case s@Some(v) => if (f(v)) s else None
  }

  @inline def exists(f: A => Boolean): Boolean = fold(false, f(_))

}

case object None extends Option[Nothing]
final case class Some[A](value: A) extends Option[A]
