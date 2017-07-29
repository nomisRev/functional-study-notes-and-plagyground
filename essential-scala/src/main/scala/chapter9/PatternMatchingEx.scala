package chapter9


object Positive {
  def unapply(arg: Int): Option[Int] = if (0 < arg) Some(arg) else None
}

object Titlecase {
  def unapply(arg: String): Option[String] = {
    val uppercased = arg.split(" ").map(a => a.capitalize).mkString(" ")
    if (uppercased.isEmpty) None else Some(uppercased)
  }
}