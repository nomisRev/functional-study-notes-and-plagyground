sealed class MathExpr2 {
    fun eval(): Result = when (this) {

        is Add2 -> {
            val ll = left.eval()
            when (ll) {
                is Failure -> ll
                is Success -> {
                    val rr = right.eval()
                    when (rr) {
                        is Failure -> rr
                        is Success -> Success(ll.value + rr.value)
                    }
                }
            }
        }

        is Sub2 -> {
            val ll = left.eval()
            when (ll) {
                is Failure -> ll
                is Success -> {
                    val rr = right.eval()
                    when (rr) {
                        is Failure -> rr
                        is Success -> Success(ll.value - rr.value)
                    }
                }
            }
        }

        is Div2 -> {
            val ll = left.eval()
            when (ll) {
                is Failure -> ll
                is Success -> {
                    val rr = right.eval()
                    when (rr) {
                        is Failure -> rr
                        is Success -> if (rr.value != 0) Success(ll.value - rr.value) else Failure("Division by zero")
                    }
                }
            }
        }
        is Number2 -> Success(value)
    }
}

data class Add2(val left: MathExpr2, val right: MathExpr2) : MathExpr2()
data class Sub2(val left: MathExpr2, val right: MathExpr2) : MathExpr2()
data class Div2(val left: MathExpr2, val right: MathExpr2) : MathExpr2()
data class Number2(val value: Int) : MathExpr2()


sealed class Result2 {
    fun flatMap(f: (Int) -> Result2): Result2 = when(this) {
        is Success2 -> f(value)
        is Failure2 -> this
    }
    fun map(f: (Int) -> Int): Result2 = when(this) {
        is Success2 -> Success2(f(value))
        is Failure2 -> this
    }
}
data class Success2(val value: Int) : Result2()
data class Failure2(val message: String) : Result2()