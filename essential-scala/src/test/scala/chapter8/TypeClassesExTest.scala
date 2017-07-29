package chapter8

import chapter7.{Equal, Person, Rational}
import org.scalatest.{FlatSpec, Matchers}


class TypeClassesExTest extends FlatSpec with Matchers {

  "When doing something without implicits it" should "work" in {
    val absOrdering = Ordering.fromLessThan[Int] { (x, y) =>
      Math.abs(x) < Math.abs(y)
    }
    assert(List(-4, -1, 0, 2, 3).sorted(absOrdering) == List(0, -1, 2, 3, -4))
    assert(List(-4, -3, -2, -1).sorted(absOrdering) == List(-1, -2, -3, -4))
  }

  "When doing something with implicits it" should "work" in {
    implicit val absOrdering = Ordering.fromLessThan[Int] { (x, y) =>
      Math.abs(x) < Math.abs(y)
    }
    assert(List(-4, -1, 0, 2, 3).sorted == List(0, -1, 2, 3, -4))
    assert(List(-4, -3, -2, -1).sorted == List(-1, -2, -3, -4))
  }

  "When ordering rational numbers with implicits it" should "work" in {
    implicit val rationalOrdering = Ordering.fromLessThan[Rational] { (x, y) =>
      (x.numerator / x.denominator) < (y.numerator / y.denominator)
    }

    assert(List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted ==
      List(Rational(1, 3), Rational(1, 2), Rational(3, 4)))
  }

  "When using a type class to test for equality it" should "work" in {
    import chapter7.Equal.EqualOps

    implicit val emailEquals = new Equal[Person] {
      override def eq(first: Person, second: Person): Boolean = first.email == second.email
    }

    assert(Person("John", "JohnAndJane@gmail.com") isEqual Person("Jane", "JohnAndJane@gmail.com"))
    assert(Equal(Person("John", "JohnAndJane@gmail.com"), Person("Jane", "JohnAndJane@gmail.com")))
    assert(Equal[Person].eq(Person("John", "JohnAndJane@gmail.com"), Person("Jane", "JohnAndJane@gmail.com")))
  }

  "When using a type class to test for equality names it" should "work" in {
    import chapter7.Equal.EqualOps

    implicit val nameEquals = new Equal[Person] {
      override def eq(first: Person, second: Person): Boolean = first.name == second.name
    }

    assert(Person("Jane", "IloveJohn@gmail.com") isEqual Person("Jane", "JohnAndJane@gmail.com"))
    assert(Equal(Person("Jane", "IloveJohn@gmail.com"), Person("Jane", "JohnAndJane@gmail.com")))
    assert(Equal[Person].eq(Person("Jane", "IloveJohn@gmail.com"), Person("Jane", "JohnAndJane@gmail.com")))
  }

}