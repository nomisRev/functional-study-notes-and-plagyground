package functional

// Define the usual Either API.
//
// * Constructors (on the object)
// left
// right
// * methods
// map
// flatMap
// orElse
// getOrElse
// any other useful functions

sealed trait Either[+A, +B] {

  def fold[X](left: A => X, right: B => X): X = this match {
    case Left(v) => left(v)
    case Right(v) => right(v)
  }

  def map[Y](f: B => Y): Either[A, Y] = this match {
    case l@Left(_) => l
    case Right(v) => Right(f(v))
  }

  def flatMap[AA >: A, Y](f: B => Either[AA, Y]): Either[AA, Y] = this match {
    case l@Left(_) => l
    case Right(v) => f(v)
  }

  def orElse[AA >: A, Y](f: => Either[AA, Y]): Either[AA, Y] = flatMap(_ => f)

  def getOrElse[C >: B](f: => C): C = fold(_ => f, a => a)

  def swap: Either[B, A] = this match {
    case Left(v) => Right(v)
    case Right(v) => Left(v)
  }

}

final case class Left[A](value: A) extends Either[A, Nothing]
final case class Right[B](value: B) extends Either[Nothing, B]

