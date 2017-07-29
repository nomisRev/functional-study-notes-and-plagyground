package funkotlin

sealed class Try<out T> {

    data class Success<T>(val value: T) : Try<T>()
    data class Failure(val throwable: Throwable) : Try<Nothing>()

    fun <R> fold(g: (Throwable) -> R, f: (T) -> R): R = when (this) {
        is Success -> f(value)
        is Failure -> g(throwable)
    }

    fun <R> flatMap(f: (T) -> Try<R>): Try<R> = fold(
            { this as Failure },
            { f(it) }
    )

    fun <R> map(f: (T) -> R): Try<R> = flatMap { Success(f(it)) }

    fun foreach(f: (T) -> Unit): Unit = fold({}, f)

    fun exists(f: (T) -> Boolean): Boolean = fold(
            { false },
            { f(it) }
    )

    val isSuccess = fold(
            { false },
            { true }
    )

    val isFailed = !isSuccess

    fun toOption(): Option<T> = fold(
            { Option.None },
            { Option.Some(it) }
    )

}

fun <T> Try<T>.filter(f: (T) -> Boolean): Try<T> = fold(
        { this },
        {
            val value: Try<T> = try {
                if (f(it)) {
                    this as Try.Success<T>
                } else {
                    Try.Failure(NoSuchElementException("Predicate does not hold for $it"))
                }
            } catch (t: Throwable) {
                Try.Failure(t)
            }
            value
        }
)

fun <T> Try<T>.filterNot(f: (T) -> Boolean): Try<T> = filter { !f(it) }

fun <T> Try<T>.getOrElse(t: () -> T): T = fold(
        { t() },
        { it }
)

fun <T> Try<T>.orElse(f: () -> Try<T>): Try<T> = fold(
        { f() },
        { this }
)

fun <T> Try<T>.recover(f: (Throwable) -> Try<T>): Try<T> = fold(
        { f(it) },
        { this }
)