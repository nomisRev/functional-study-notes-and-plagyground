package chapter5

sealed trait Maybe[+A] {

  def fold[B](f: => B)(g: A => B): B = this match {
    case Full(a) => g(a)
    case Empty => f
  }

  def flatMap[B](f: A => Maybe[B]): Maybe[B] = fold(Empty: Maybe[B])(f(_))

  def map[B](f: A => B): Maybe[B] = fold(Empty: Maybe[B])(a => Full(f(a)))

  def mapByFlatMap[B](f: A => B): Maybe[B] = flatMap(a => Full(f(a)))

}
final case class Full[A](value: A) extends Maybe[A]
case object Empty extends Maybe[Nothing]