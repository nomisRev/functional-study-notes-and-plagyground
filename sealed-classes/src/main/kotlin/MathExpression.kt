sealed class MathExpression {
    fun eval(): Result = when (this) {
        is Addition -> left.eval().flatMap { ll -> right.eval().map { rr -> ll + rr } }
        is Subtraction -> left.eval().flatMap { ll -> right.eval().map { rr -> ll - rr } }
        is Division -> right.eval().flatMap { rr -> if (rr == 0)  Failure("Division by zero") else left.eval().map { ll -> ll/rr }}
        is Number -> Success(value)
    }
}

data class Addition(val left: MathExpression, val right: MathExpression) : MathExpression()
data class Subtraction(val left: MathExpression, val right: MathExpression) : MathExpression()
data class Division(val left: MathExpression, val right: MathExpression) : MathExpression()
data class Number(val value: Int) : MathExpression()


sealed class Result {
    fun flatMap(f: (Int) -> Result): Result = when(this) {
        is Success -> f(value)
        is Failure -> this
    }
    fun map(f: (Int) -> Int): Result = when(this) {
        is Success -> Success(f(value))
        is Failure -> this
    }
}
data class Success(val value: Int) : Result()
data class Failure(val message: String) : Result()