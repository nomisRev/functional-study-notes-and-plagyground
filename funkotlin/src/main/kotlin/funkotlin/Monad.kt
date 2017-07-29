package funkotlin

interface Monad<out T> {
    fun <R> pure(t: R): Monad<R>
    fun <R> flatMap(f: (T) -> Monad<R>): Monad<R>
    fun <R> map(f: (T) -> R): Monad<R> = flatMap { pure(f(it)) }
}