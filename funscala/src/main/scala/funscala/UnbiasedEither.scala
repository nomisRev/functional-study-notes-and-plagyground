package funscala

/**
  * Implementing biased either is not really interesting because it is almost identical to Try
  */
sealed trait UnbiasedEither[+L, +R] {

  @inline def fold[X](l: L => X, r: R => X): X = this match {
    case MyLeft(v) => l(v)
    case MyRight(v) => r(v)
  }

  @inline def left[LL >: L, RR >: R]: LeftProjection[LL, RR] = LeftProjection(this)

  @inline def right[LL >: L, RR >: R]: RightProjection[LL, RR] = RightProjection(this)

  @inline def isLeft: Boolean = fold(_ => true, _ => false)

  @inline def isRight: Boolean = !isLeft

}

final case class MyLeft[L, R](value: L) extends UnbiasedEither[L, R]
final case class MyRight[L, R](value: R) extends UnbiasedEither[L, R]

final case class LeftProjection[L, R](either: UnbiasedEither[L, R]) {

  @inline def flatMap[X](f: L => UnbiasedEither[X, R]): LeftProjection[X, R] = either match {
    case MyLeft(v) => LeftProjection(f(v))
    case MyRight(v) => LeftProjection(MyRight(v))
  }

  @inline def map[X](f: L => X): LeftProjection[X, R] = flatMap(a => MyLeft(f(a)))

}

final case class RightProjection[L, R](either: UnbiasedEither[L, R]) {

  @inline def flatMap[X](f: R => UnbiasedEither[L, X]): RightProjection[L, X] = either match {
    case MyLeft(v) => RightProjection(MyLeft(v))
    case MyRight(v) => RightProjection(f(v))
  }

  @inline def map[X](f: R => X): RightProjection[L, X] = flatMap(a => MyRight(f(a)))

}