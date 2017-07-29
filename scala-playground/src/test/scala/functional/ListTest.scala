package functional

import org.scalatest.{FlatSpec, Matchers}

class ListTest extends FlatSpec with Matchers {

  val example: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))

  "Mapping a list" should "result in each item being mapped" in {
    example.map(a => 2 * a) shouldBe Cons(2, Cons(4, Cons(6, Nil)))
    (Nil: List[Int]).map(a => 2 * a) shouldBe Nil
  }

  "Appending a list" should "result in two sequential lists" in {
    val example2: List[Int] = Cons(4, Cons(5, Nil))
    example ::: example2 shouldBe Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))

    0 :: example shouldBe Cons(0, Cons(1, Cons(2, Cons(3, Nil))))
  }

  "Flatmapping a list" should " result in a flat list" in {
    Cons(1, Cons(5, Nil)).flatMap(a => Cons(a, Cons(a * 2, Nil))) shouldBe Cons(1, Cons(2, Cons(5, Cons(10, Nil))))
  }

  "Filtering a list" should "remove all the items that don't meet the condition" in {
    example.filter(a => a % 2 > 0) shouldBe Cons(1, Cons(3, Nil))
  }

  "Reversing a list" should "work" in {
    example.reverse() shouldBe Cons(3, Cons(2, Cons(1, Nil)))
  }

}
