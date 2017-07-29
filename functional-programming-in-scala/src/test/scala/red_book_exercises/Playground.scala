package red_book_exercises

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks


@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class Playground extends FlatSpec with PropertyChecks {

  sealed trait IntList {
    def length: Int = this match {
      case End => 0
      case Pair(_, t) => 1 + t.length
    }

    def product: Int = this match {
      case End => 1
      case Pair(h, t) => h * t.product
    }

    def double: IntList = this match {
      case End => End
      case Pair(h, t) => Pair(2 * h, t.double)
    }
  }

  final case object End extends IntList

  final case class Pair(head: Int, tail: IntList) extends IntList

  behavior of "exercise"

  val example = Pair(1, Pair(2, Pair(3, End)))

  it should "calculate length" in {
    assert(example.length == 3)
    assert(example.tail.length == 2)
    assert(End.length == 0)
  }

  it should "calculate product" in {
    assert(example.product == 6)
    assert(example.tail.product == 6)
    assert(End.product == 1)
  }

  it should "double the list" in {
    assert(example.double == Pair(2, Pair(4, Pair(6, End))))
    assert(example.tail.double == Pair(4, Pair(6, End)))
    assert(End.double == End)
  }

  sealed trait IntTree {
    def sum: Int /*= this match {
      case Leaf(v) => v
      case Branch(l, r) => l.sum + r.sum
    }*/

    def double: IntTree /*= this match {
      case Leaf(v) => Leaf(v * 2)
      case Branch(l, r) => Branch(l.double, r.double)
    }*/
  }

  final case class Leaf(v: Int) extends IntTree {
    override def sum: Int = v

    override def double: IntTree = Leaf(v*2)
  }

  final case class Branch(l: IntTree, r: IntTree) extends IntTree {
    override def sum: Int = l.sum + r.sum

    override def double: IntTree = Branch(l.double, r.double)
  }

  val treeExample: IntTree = Branch(
    Branch(
      Leaf(1),
      Leaf(1)
    ),
    Branch(
      Leaf(1),
      Leaf(1)
    )
  )

  it should "calculate sum" in {
    assert(treeExample.sum == 4)
    assert(Branch(Leaf(2), Leaf(2)).sum == 4)
    assert(Leaf(5).sum == 5)
  }

  it should "double the tree" in {
    assert(treeExample.double == Branch(
      Branch(
        Leaf(2),
        Leaf(2)
      ),
      Branch(
        Leaf(2),
        Leaf(2)
      )
    ))
    assert(Branch(Leaf(2), Leaf(2)).double == Branch(Leaf(4), Leaf(4)))
    assert(Leaf(5).double == Leaf(10))
  }

}