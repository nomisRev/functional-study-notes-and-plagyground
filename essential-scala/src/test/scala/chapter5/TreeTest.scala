package chapter5

import org.scalatest.{FlatSpec, Matchers}

class TreeTest extends FlatSpec with Matchers {

  val tree: Tree[String] =
    Node(
      Node(
        Leaf("To"),
        Leaf("iterate")
      ),
      Node(
        Node(
          Leaf("is"),
          Leaf("human,")
        ),
        Node(
          Leaf("to"),
          Node(Leaf("recurse"), Leaf("divine")
          )
        )
      )
    )

  "folding a tree of strings" should "become a full sentence" in {
    tree.fold(a => a)((a,b) => a + " " + b) shouldBe "To iterate is human, to recurse divine"
  }

}
