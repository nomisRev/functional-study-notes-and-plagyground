package chapter7

import scala.language.implicitConversions

final case class Rational(numerator: Double, denominator: Double)

final case class Order(units: Int, unitPrice: Double) {
  val totalPrice: Double = units * unitPrice
}

object Order {
  implicit val ordering: Ordering[Order] = Ordering.fromLessThan[Order] { (a, b) =>
    a.totalPrice < b.totalPrice
  }
}

object OrderByUnits {
  implicit val ordering: Ordering[Order] = Ordering.fromLessThan[Order] { (a, b) =>
    a.units < b.units
  }
}

object OrderByUnitPrice {
  implicit val ordering: Ordering[Order] = Ordering.fromLessThan[Order] { (a, b) =>
    a.unitPrice < b.unitPrice
  }
}

final case class Person(name: String, email: String)

trait Equal[A] {
  def eq(first: A, second: A): Boolean
}

object Equal {
  implicit class EqualOps[A](val a: A) extends AnyVal {
    def isEqual(other: A)(implicit equal: Equal[A]): Boolean = equal.eq(a, other)

    def ===(other: A)(implicit equal: Equal[A]): Boolean = isEqual(other)(equal)
  }

  def apply[A](first: A, second: A)(implicit equal: Equal[A]): Boolean = equal.eq(first, second)

  def apply[A](implicit equal: Equal[A]): Equal[A] = equal

}


object Int {

  //BE VERY CAREFUL WITH implicitConversions !!
  implicit def intToIntOps(i: Int): IntOps = new IntOps(i)

  /*implicit*/ class IntOps(val int: Int) extends AnyVal {
    def yeah(): Unit = times(_ => println("Oh yeah"))

    def times(f: Int => Unit): Unit = if (int > 0) Range(0, int).foreach(i => f(i))
  }
}