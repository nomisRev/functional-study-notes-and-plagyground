package chapter5

import org.scalatest._


class LinkedListTest extends FlatSpec with Matchers {

  val example = Cons(1, Cons(2, Cons(3, Nil)))

  "Getting the size of a linked list" should "return the actual length" in {
    example.length shouldBe 3
    example.tail.length shouldBe 2
    Nil.length shouldBe 0

    example.lengthByFold shouldBe 3
    example.tail.lengthByFold shouldBe 2
    Nil.lengthByFold shouldBe 0
  }

  it should "return true if the item is found in the list" in {
    example.contains(3) shouldBe true
    example.contains(4) shouldBe false
    Nil.contains(0) shouldBe false
  }

  "Getting an item at a certain index" should "return the item or end in an exception" in {
    //    example(0) shouldBe 1
    //    example(1) shouldBe 2
    //    example(2) shouldBe 3
    //
    //    the[Exception] thrownBy {
    //      example(3)
    //    } should have message "Bad things happened"

    example(0) shouldBe Success(1)
    example(1) shouldBe Success(2)
    example(2) shouldBe Success(3)
    example(3) shouldBe Failure("Index out of bounds")
  }

  "Doubling a list of ints" should "multiply all ints by 2" in {
    LinkedList.double(example) shouldBe LinkedList(2, 4, 6)
    LinkedList.double(Nil) shouldBe Nil
  }

  "Summing a list of ints" should "sum all the ints" in {
    LinkedList.sum(example) shouldBe 6
    LinkedList.sum(Nil) shouldBe 0
  }

  "Producting a list of ints" should "multiply all the ints" in {
    LinkedList.product(example) shouldBe 6
    LinkedList.product(Nil) shouldBe 1
  }

  "Flatmapping ints to there positive and negative value" should "result in a list double as long" in {
    LinkedList(1, 2, 3, 4).flatMap(a => LinkedList(-a, a)) shouldBe LinkedList(-1, 1, -2, 2, -3, 3, -4, 4)
  }

  "Mapping uneven maybe to None" should "result in a new list" in {
    LinkedList(Full(3), Full(2), Full(1)).map { full =>
      full.flatMap { i => if (i % 2 == 0) Full(i) else Empty }
    } shouldBe LinkedList(Empty, Full(2), Empty)
  }

}
