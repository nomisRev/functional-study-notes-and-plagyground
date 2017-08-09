package funscala

sealed trait Try[+T] extends MonadMethod[Try, T] {

  override def identity[TT >: T]: Try[TT] = Failure(new IllegalArgumentException("Element doesn't exist"))

  @inline def fold[R](f: => R, g: T => R): R = this match {
    case Failure(_) => f
    case Success(v) => g(v)
  }

  @inline override def flatMap[R](f: T => Try[R]): Try[R] = this match {
    case f@Failure(_) => f
    case Success(v) => f(v)
  }

  @inline def isSuccess: Boolean = fold(false, _ => true)

  @inline def isFailed: Boolean = !isSuccess

  @inline def toOption: Option[T] = fold(None, Some(_))

  @inline def recover[TT >: T](f: => Try[TT]): Try[TT] = fold(f, _ => this)
  
}

object Try {

  implicit def pureable[T] = new Pureable[Try,T] {
    override def pure[X](x: X): Try[X] = Success(x)
  }

}

final case class Success[T](value: T) extends Try[T]
final case class Failure(throwable: Throwable) extends Try[Nothing]