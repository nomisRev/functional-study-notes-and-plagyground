package chapter9

import org.scalatest.{FlatSpec, FunSuite, Matchers}


class PatternMatchingExTest extends FlatSpec with Matchers {

  "When pattern matching this " should "just work" in {
    assert(
      "No" ==
        (0 match {
          case Positive(_) => "Yes"
          case _ => "No"
        })
    )

    assert(
      "Yes" ==
        (42 match {
          case Positive(_) => "Yes"
          case _ => "No"
        })
    )

    assert(
      "Sir Lord Doctor David Gurnell" ==
        ("sir lord doctor david gurnell" match {
          case Titlecase(str) => str
        })
    )
  }

}
