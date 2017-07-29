package red_book_exercises.chapter_3_functional_data_structures

import red_book_exercises.chapter_3_functional_data_structures.List.tail
import red_book_exercises.chapter_3_functional_data_structures.List.setHead
import red_book_exercises.chapter_3_functional_data_structures.List.drop
import red_book_exercises.chapter_3_functional_data_structures.List.dropWhile
import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import scala.{List => SList}

/**
  * Unit tests based on work by Munich Functional Programming in Scala reading group
  */
@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ListSpec extends FlatSpec with PropertyChecks{

  def head[A](as: List[A]) = as match {
    case Nil => sys.error("head of empty list")
    case Cons(h, _) => h
  }

  private implicit def arbList[T](implicit ev: Arbitrary[Array[T]]): Arbitrary[List[T]] =
    Arbitrary(for {
      as <- arbitrary[Array[T]]
    } yield List(as: _*))

  private def arbListTuple[T](implicit ev: Arbitrary[Array[T]]): Arbitrary[(List[T],SList[T])] =
    Arbitrary(for {
      as <- arbitrary[Array[T]]
    } yield (List(as: _*), as.toList))

  private def toList[A](l: SList[A]) = List(l: _*)

  behavior of "tail"

  it should "return Nil when getting passed an empty list" in {
      assertResult(Nil)(tail(Nil))
  }

  it should "work" in {
    def testTail[A,B](as: List[A], expected: B) = assertResult(expected)(tail(as))

    val tests = Table(
      ("as", "tail(as)"),
      (List(0), Nil),
      (List(0, 1), List(1)),
      (List("a"), Nil),
      (List("a", "b"), List("b")))
    forAll(tests)(testTail)
  }

  it should "for all as: List[Int] ==> Cons(head(as), tail(as)) == as" in {
    forAll("as") { as: List[Int] =>
      whenever(as != Nil) {
        assertResult(Cons(head(as), tail(as)))(as)
      }
    }
  }

  it should "for all as: List[String] ==> Cons(head(as), tail(as)) == as" in {
    forAll("as") { as: List[String] =>
      whenever(as != Nil) {
        assertResult(Cons(head(as), tail(as)))(as)
      }
    }
  }

  it should "be equivalent to Scala List function" in {
    forAll(arbListTuple[Int].arbitrary) { case (l, sl) =>
      whenever(!sl.isEmpty) {
        assertResult(toList(sl.tail))(tail(l))
      }
    }
  }

  behavior of "setHead"

  it should "return Nil when getting passed an empty List" in {
    assertResult(Nil)(setHead(Nil, 1))
  }

  it should "work" in {
    def testSetHead[A](as: List[A], h: A, expected: List[A]) =
      assertResult(expected)(setHead(as, h))

    val tests = Table(
      ("as", "h", "setHead(as, h)"),
      (List(0), 1, List(1)),
      (List(0, 1), 2, List(2, 1)),
      (List("a"), "b", List("b")),
      (List("a", "b"), "c", List("c", "b")))
    forAll(tests)(testSetHead)
  }

  it should "for all as: List[Int] ==> setHead(as, head(as)) == as" in {
    forAll("as") { as: List[Int] =>
      whenever(as != Nil) {
        assertResult(setHead(as, head(as)))(as)
      }
    }
  }

  it should "for all as: List[String] ==> setHead(as, head(as)) == as" in {
    forAll("as") { as: List[String] =>
      whenever(as != Nil) {
        assertResult(setHead(as, head(as)))(as)
      }
    }
  }

  it should "for all as: (List[Int], h: Int) ==> setHead(as, h) == Cons(h, tail(as))" in {
    forAll("as", "h") { (as: List[Int], h: Int) =>
      whenever(as != Nil) {
        assertResult(setHead(as, h))(Cons(h, tail(as)))
      }
    }
  }

  it should "for all as: List[String], h: String ==> setHead(as, h) == Cons(h, tail(as))" in {
    forAll("as", "h") { (as: List[String], h: String) =>
      whenever(as != Nil) {
        assertResult(setHead(as, h))(Cons(h, tail(as)))
      }
    }
  }


  behavior of "drop"

  it should "work" in {
    def testDrop[A](as: List[A], n: Int, expected: List[A]) =
      assertResult(expected)(drop(as, n))
    val l123 = List(1,2,3)
    val labc = List("a","b","c")

    val tests = Table(
      ("l", "n", "drop(l, n)"),
      (Nil, 1, Nil),
      (Nil, 2, Nil),

      (l123, Int.MinValue, l123),
      (l123, -1, l123),
      (l123, 0, l123),
      (l123, 1, List(2,3)),
      (l123, 2, List(3)),
      (l123, 3, Nil),
      (l123, Int.MaxValue, Nil),

      (labc, 1, List("b","c")),
      (labc, 2, List("c")),
      (labc, 3, Nil))
    forAll(tests)(testDrop)
  }

  behavior of "length"

  it should "for all l: List[Int] ==> length(drop(l, n)) == length(l) - n" in {
    forAll("l") { l: List[Int] =>
      val len = List.length(l)
      forAll (Gen.chooseNum(0, len)) { n: Int =>
        assertResult(len - n)(List.length(drop(l, n)))
      }
    }
  }

  behavior of "length2"

  it should "for all l: List[Int] ==> length(drop(l, n)) == length(l) - n" in {
    forAll("l") { l: List[Int] =>
      val len = List.length(l)
      forAll (Gen.chooseNum(0, len)) { n: Int =>
        assertResult(len - n)(List.length2(drop(l, n)))
      }
    }
  }

  behavior of "dropWhile"

  it should "work" in {
    def testDropWhileOdd(l: List[Int], expected: List[Int]) = {
      def odd(n: Int) = n % 2 != 0
      assertResult(expected)(dropWhile(l, odd))
    }

    val tests = Table(
      ("l", "dropWhile(l, odd)"),
      (Nil, Nil),
      (List(1, 3, 1, 2), List(2)),
      (List(2, 4), List(2, 4)),
      (List(2, 1), (List(2, 1))))
    forAll(tests)(testDropWhileOdd)
  }


  behavior of "init"

  it should "return Nil when passed an empty List" in {
    assertResult(Nil)(List.init(Nil))
  }

  it should "work" in {
    def testInit[A](l: List[A], expected: List[A]) = {
      assertResult(expected)(List.init(l))
    }

    val tests = Table(
      ("l", "init(l)"),
      (List(1), Nil),
      (List(1, 2, 3), List(1, 2)),
      (List("a"), Nil),
      (List("a", "b", "c"), List("a", "b")))
    forAll(tests)(testInit)
  }

  behavior of "init2"

  it should "return Nil when passed an empty List" in {
    assertResult(Nil)(List.init2(Nil))
  }

  it should "work" in {
    def testInit2[A](l: List[A], expected: List[A]) = {
      assertResult(expected)(List.init2(l))
    }

    val tests = Table(
      ("l", "init(l)"),
      (List(1), Nil),
      (List(1, 2, 3), List(1, 2)),
      (List("a"), Nil),
      (List("a", "b", "c"), List("a", "b")))
    forAll(tests)(testInit2)
  }


}