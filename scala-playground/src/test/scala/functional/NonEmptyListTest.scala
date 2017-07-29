package functional

import org.scalatest.{FlatSpec, FunSuite, Matchers}

class NonEmptyListTest extends FlatSpec with Matchers {

  "Reversing a non empty list" should "reverse" in {
    NECons(1, NECons(2, Single(3))).reverse() shouldBe NECons(3, NECons(2, Single(1)))
  }

}
