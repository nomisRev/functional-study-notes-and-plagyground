package funscala

trait Functor[F[_], O[_], T] {
  def flatMap[X](f: T => F[X]): F[X]
  def map[TT >: T, X](f: T => X)(implicit pureable: Pureable[F,TT]): F[X] = flatMap(t => pureable.pure(f(t)))
}

trait Pureable[F[_], T] {
  def pure[X](x: X): F[X]
}

trait MonadMethod[F[_], +T] extends Functor[F,T] {

  def identity[TT >: T]: F[TT]

  @inline def fold[R](n: => R, s: T => R): R

  @inline def foreach(f: T => Unit): Unit = fold(_: Nothing => Unit, f)

  @inline def filter[TT >: T](f: TT => Boolean)(implicit pureable: Pureable[F,TT]): F[TT] = flatMap { t =>
    if (f(t)) pureable.pure(t)
    else identity
  }

  @inline def filterNot[TT >: T](f: TT => Boolean)(implicit pureable: Pureable[F,TT]): F[TT] = filter[TT](a => !f(a))(pureable)

  @inline def exists[TT >: T](f: => TT): Boolean = fold(false, a => a == f)

  @inline def contains[TT >: T](f: => TT): Boolean = exists((a: T) => a == f)

  @inline def orElse[TT >: T](f: => F[TT])(implicit pureable: Pureable[F,TT]): F[TT] = fold(f, a => pureable.pure(a))

  @inline def getOrElse[TT >: T](f: => TT): TT = fold(f, a => a)
}