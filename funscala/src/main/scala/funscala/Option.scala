package funscala

sealed trait Option[+T] extends MonadMethod[Option,T] {

  override def identity[TT >: T]: Option[TT] = None

  @inline def fold[R](n: => R, s: T => R): R = this match {
    case None => n
    case Some(v) => s(v)
  }

  @inline override def flatMap[R](f: T => Option[R]): Option[R] = fold(None, f(_))

  @inline def isAbsent: Boolean = fold(true, _ => false)

  @inline def isPresent: Boolean = !isAbsent

}

object Option {

  implicit def pureable[T] = new Pureable[Option,T] {
    override def pure[X](x: X): Option[X] = Some(x)
  }

}

final case class Some[T](value: T) extends Option[T]
case object None extends Option[Nothing]

