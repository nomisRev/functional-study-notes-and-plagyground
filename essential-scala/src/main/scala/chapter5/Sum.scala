package chapter5

sealed trait Sum[+A, +B] {

  def fold[C](f: A => C)(g: B => C): C = this match {
    case Failure(a) => f(a)
    case Success(b) => g(b)
  }

  def map[C](f: B => C): Sum[A, C] = this match {
    case fail@Failure(_) => fail
    case Success(b) => Success(f(b))
  }

  def flatMap[AA>: A, C](f: B => Sum[AA,C]): Sum[AA, C] = this match {
    case fail@Failure(_) => fail
    case Success(b) => f(b)
  }

}

final case class Failure[A](value: A) extends Sum[A, Nothing]
final case class Success[B](value: B) extends Sum[Nothing, B]