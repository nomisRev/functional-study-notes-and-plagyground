package chapter6


object RangeEx {

  val subjects = List("Noel", "The cat", "The dog")

  val verbs = List("wrote", "chased", "slept on")

  val objects = List("the book", "the ball", "the bed")

  val verbsFor = Map(
    "Noel" -> List("wrote", "chased", "slept on"),
    "The cat" -> List("meowed at", "chased", "slept on"),
    "The dog" -> List("barked at", "chased", "slept on")
  )

  val objectsFor = Map(
    "wrote" -> List("the book", "the letter", "the code"),
    "chased" -> List("the ball", "the dog", "the cat"),
    "slept on" -> List("the bed", "the mat", "the train"),
    "meowed at" -> List("Noel", "the door", "the food cupboard"),
    "barked at" -> List("the postman", "the car", "the cat")
  )

  def verbsFor(subject: String): List[String] =
    subject match {
      case "Noel" => List("wrote", "chased", "slept on")
      case "The cat" => List("meowed at", "chased", "slept on")
      case "The dog" => List("barked at", "chased", "slept on")
    }

  def objectsFor(verb: String): List[String] =
    verb match {
      case "wrote" => List("the book", "the letter", "the code")
      case "chased" => List("the ball", "the dog", "the cat")
      case "slept on" => List("the bed", "the mat", "the train")
      case "meowed at" => List("Noel", "the door", "the food cupboard")
      case "barked at" => List("the postman", "the car", "the cat")
    }

  val combinations: Seq[(String, String, String)] = for {
    subject <- subjects
    verb <- verbs
    obj <- objects
  } yield (subject, verb, obj)

  //Since I used maps instead of a pattern matching method you have to deal with optional gets of map
  //Only use maps for something like that if the content can change at runtime because it comes with code overhead and runtime overhead.
  val betterCombinations: Seq[(String, String, String)] = for {
    sub <- subjects
    verb <- verbsFor.getOrElse(sub, Seq.empty)
    obj <- objectsFor.getOrElse(verb, Seq.empty)
  } yield (sub, verb, obj)

  //    subjects.flatMap { sub =>
  //      verbsFor.getOrElse(sub, Seq.empty).flatMap { verb =>
  //        objectsFor.getOrElse(verb, Seq.empty).map { obj => (sub, verb, obj) }
  //      }
  //    }

}


final case class Distribution[A](events: List[(A, Double)]) {
  def map[B](f: A => B): Distribution[B] =
    Distribution(events map { case (a, p) => f(a) -> p })

  def flatMap[B](f: A => Distribution[B]): Distribution[B] =
    Distribution(events flatMap { case (a, p1) =>
      f(a).events map { case (b, p2) => b -> (p1 * p2) }
    }).compact.normalize

  def normalize: Distribution[A] = {
    val totalWeight = (events map { case (a, p) => p }).sum
    Distribution(events map { case (a,p) => a -> (p / totalWeight) })
  }

  def compact: Distribution[A] = {
    val distinct = (events map { case (a, p) => a }).distinct
    def prob(a: A): Double =
      (events filter { case (x, p) => x == a } map { case (a, p) => p }).sum

    Distribution(distinct map { a => a -> prob(a) })
  }
}
object Distribution {
  def uniform[A](atoms: List[A]): Distribution[A] = {
    val p = 1.0 / atoms.length
    Distribution(atoms.map(a => a -> p))
  }
}

object CoinTossing {
  sealed trait Coin
  final case object Heads extends Coin
  final case object Tails extends Coin

  val fairCoin: Distribution[Coin] = Distribution.uniform(List(Heads, Tails))
  val threeFlips =
    for {
      c1 <- fairCoin
      c2 <- fairCoin
      c3 <- fairCoin
    } yield (c1, c2, c3)
}