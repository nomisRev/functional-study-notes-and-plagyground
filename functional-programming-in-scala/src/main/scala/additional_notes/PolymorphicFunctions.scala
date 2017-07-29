package additional_notes

import additional_notes.PolymorphicFunctions.Plus

object PolymorphicFunctions {

  def head[A](xs: List[A]): A = xs(0)

  trait Plus[A] {
    def plus(a2: A): A
  }

  def plus[A <: Plus[A]](a1: A, a2: A): A = a1.plus(a2)


  trait Plus2[A] {
    def plus(a1: A, a2: A): A
  }

  object IntPlus2 extends Plus2[Int] {
    override def plus(a1: Int, a2: Int): Int = a1 + a2
  }

  implicit val intPlus2 = IntPlus2

  def plus[A](a1: A, a2: A)(implicit p: Plus2[A]): A = p.plus(a1, a2)

}

case class MyOwnType(message: String) extends Plus[MyOwnType] {

  override def plus(a2: MyOwnType): MyOwnType = MyOwnType(message + a2.message)

}