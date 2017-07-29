package chapter5

sealed trait Expression {

  def eval: Sum[String, Double] = {
    def lift2(l: Expression, r: Expression, f: (Double, Double) => Sum[String, Double]) =
      l.eval flatMap { left =>
        r.eval flatMap { right =>
          f(left, right)
        }
      }

    this match {
      case Number(v) => Success(v)
      case Addition(l, r) => lift2(l, r, (ll, rr) => Success(ll + rr))
      case Subtraction(l, r) => lift2(l, r, (ll, rr) => Success(ll - rr))
      case Division(l, r) => lift2(l, r, (ll, rr) => if (rr < 1) Failure("Division by zero") else Success(ll / rr))
      case SquareRoot(l) => l.eval.flatMap(ll => if (ll < 0) Failure("Square root of negative number") else Success(Math.sqrt(ll)))
    }
  }

}

final case class Addition(left: Expression, right: Expression) extends Expression
final case class Subtraction(left: Expression, right: Expression) extends Expression
final case class Division(left: Expression, right: Expression) extends Expression
final case class SquareRoot(value: Expression) extends Expression
final case class Number(value: Double) extends Expression
