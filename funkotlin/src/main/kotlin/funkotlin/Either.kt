package funkotlin

sealed class Either<out L, out R> {

    fun <X> fold(f: (L) -> X, g: (R) -> X) = when (this) {
        is Left -> f(l)
        is Right -> g(r)
    }

    val isLeft: Boolean = fold({ true }, { false })
    val isRight: Boolean = !isLeft

    fun left() = LeftProjection(this)

}

data class Left<out L, out R>(val l: L) : Either<L, R>()
data class Right<out L, out R>(val r: R) : Either<L, R>()

data class LeftProjection<out L, out R>(val either: Either<L, R>) {

    fun <X> map(f: (L) -> X): LeftProjection<X, R> = flatMap { Left<X, R>(f(it)) }

}

fun <L, R, X> LeftProjection<L, R>.flatMap(f: (L) -> Either<X, R>): LeftProjection<X, R> = when (either) {
    is Left -> LeftProjection(f(either.l))
    is Right -> LeftProjection(Right(either.r))
}

data class RightProjection<out L, out R>(val either: Either<L, R>) {

    fun <X> map(f: (R) -> X): RightProjection<L, X> = flatMap { Right<L, X>(f(it)) }

}

fun <L, R, X> RightProjection<L, R>.flatMap(f: (R) -> Either<L, X>): RightProjection<L, X> = when (either) {
    is Left -> RightProjection(Left(either.l))
    is Right -> RightProjection(f(either.r))
}