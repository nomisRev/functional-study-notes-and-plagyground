package chapter5

import org.scalatest.{FlatSpec, Matchers}

class ExpressionTest extends FlatSpec with Matchers {

  "When making calculations with Expressions" should "result in a success or failure" in {
    Addition(Number(1), Number(2)).eval shouldBe Success(3)
    SquareRoot(Number(-1)).eval shouldBe Failure("Square root of negative number")
    Division(Number(4), Number(0)).eval shouldBe Failure("Division by zero")
    Division(Addition(Subtraction(Number(8), Number(6)), Number(2)), Number(2)).eval shouldBe Success(2.0)

  }

}