package funkotlin

sealed class Option<out T> {

    data class Some<out T>(val value: T) : Option<T>()
    object None : Option<Nothing>()

    inline fun <R> fold(g: () -> R, f: (T) -> R): R = when (this) {
        is Some -> f(value)
        None -> g()
    }

    inline fun <R> flatMap(f: (T) -> Option<R>): Option<R> = fold(
            { None },
            { f(it) }
    )

    inline fun <R> map(f: (T) -> R): Option<R> = flatMap { Some(f(it)) }

    inline fun foreach(f: (T) -> Unit): Unit = fold({}, f)

    inline fun filter(f: (T) -> Boolean): Option<T> = fold(
            { None },
            { if (f(it)) Some(it) else None }
    )

    inline fun filterNot(f: (T) -> Boolean): Option<T> = filter { !f(it) }

    inline fun exists(f: (T) -> Boolean): Boolean = fold(
            { false },
            { f(it) }
    )

}

inline fun <T> Option<T>.getOrElse(f: () -> T): T = fold(
        { f() },
        { it }
)

inline fun <T> Option<T>.orElse(f: () -> Option<T>): Option<T> = fold(
        { f() },
        { this }
)

inline fun <T> Option<T>.contains(f: () -> T): Boolean = exists { it == f() }
