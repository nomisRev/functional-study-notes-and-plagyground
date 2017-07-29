package chapter6

import scala.collection.convert.Wrappers.MutableSeqWrapper

object CollectionExercices {
  val seq = Seq("Cat", "Dog", "Penguin")

  val seq2 = "Mouse" +: seq :+ "Tyrannosaurus"

  //The type of the list will change to any because we create a new Seq[_] with as parameters Any which is the first common parent of Int and String
  val anySeq = 2 +: seq

  val memento = Film("Memento", 2000, 8.5)
  val darkKnight = Film("Dark Knight", 2008, 9.0)
  val inception = Film("Inception", 2010, 8.8)

  val highPlainsDrifter = Film("High Plains Drifter", 1973, 7.7)
  val outlawJoseyWales = Film("The Outlaw Josey Wales", 1976, 7.9)
  val unforgiven = Film("Unforgiven", 1992, 8.3)
  val granTorino = Film("Gran Torino", 2008, 8.2)
  val invictus = Film("Invictus", 2009, 7.4)

  val predator = Film("Predator", 1987, 7.9)
  val dieHard = Film("Die Hard", 1988, 8.3)
  val huntForRedOctober = Film("The Hunt for Red October", 1990, 7.6)
  val thomasCrownAffair = Film("The Thomas Crown Affair", 1999, 6.8)

  val eastwood = Director("Clint", "Eastwood", 1930,
    Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino, invictus))

  val mcTiernan = Director("John", "McTiernan", 1951,
    Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair))

  val nolan = Director("Christopher", "Nolan", 1970,
    Seq(memento, darkKnight, inception))

  val someGuy = Director("Just", "Some Guy", 1990,
    Seq())

  val directors = Seq(eastwood, mcTiernan, nolan, someGuy)

}

final case class Film(
                       name: String,
                       yearOfRelease: Int,
                       imdbRating: Double
                     )

final case class Director(
                           firstName: String,
                           lastName: String,
                           yearOfBirth: Int,
                           films: Seq[Film]
                         )

object Director {
  def hasNumerOfFilmsUnderHisBelt(directors: Seq[Director], num: Int): Seq[Director] = directors.filter(_.films.length > num)

  def bornBefore(directors: Seq[Director], year: Int): Option[Director] = directors.find(_.yearOfBirth < year)

  def numberOfMoviesAndBornBefore(directors: Seq[Director], num: Int, year: Int): Seq[Director] = hasNumerOfFilmsUnderHisBelt(directors, num).filter(_.yearOfBirth < year)

  def soryByYearOfBirth(directors: Seq[Director], ascending: Boolean = false): Seq[Director] =
    if (ascending) directors.sortWith((d1, d2) => d1.yearOfBirth < d2.yearOfBirth)
    else directors.sortWith((d1, d2) => d1.yearOfBirth > d2.yearOfBirth)

  import CollectionExercices._

  val moviesByNolan: Seq[String] = nolan.films.map(_.name)

  val allMovies: Seq[String] = directors.flatMap(_.films.map(_.name))

  val earliestMcTiernanMovie: Option[Film] = mcTiernan.films.sortWith { (f1, f2) => f1.yearOfRelease > f2.yearOfRelease }.headOption
  //mcTiernan.films.foldLeft(Int.MaxValue) { (current, film) => math.min(current, film.yearOfRelease) }

  val sortedMovies: Seq[Film] = directors.flatMap(_.films).sortWith { (f1, f2) => f1.imdbRating > f2.imdbRating }

  val average: Double = {
    val films = directors.flatMap(director => director.films)
    films.foldLeft(0.0)((a, b) => a + b.imdbRating) / films.length
  }

  //Consider using foreach instead of flatMap/map to avoid accumulating a result.
  directors.flatMap(a =>
    a.films.map(b =>
      s"Tonight only! ${b.name} by ${a.firstName} ${a.lastName}!\n"
    )
  ).foreach(print(_))

  val earliestMovie: Option[Film] = directors.flatMap(_.films).sortWith { (a, b) => a.yearOfRelease < b.yearOfRelease }.headOption

  def min(seq: Seq[Int]): Option[Int] = seq.sortWith((a, b) => a < b).headOption

  //  def smallest(seq: Seq[Int]): Int = seq.foldLeft(Int.MaxValue)(math.min)

  def smallest(seq: Seq[Int]): Option[Int] =
    seq.foldRight(None: Option[Int]) { (a, b) =>
      Some(a).map(aa => b match {
        case None => aa
        case Some(bb) => math.min(aa, bb)
      })
    }

  def unique(seq: Seq[Int]): Seq[Int] = {
    def insert(seq: Seq[Int], el: Int): Seq[Int] = if (seq.contains(el)) seq else seq :+ el

    seq.foldLeft(Seq[Int]())((b, a) => insert(b, a))
    //    seq.foldLeft(Seq.empty[Int])(insert)
  }

  def reverse[A](seq: Seq[A]): Seq[A] = seq.foldLeft(Seq.empty[A]) { (b, a) => a +: b }

  def mapByFoldRight[A, B](seq: Seq[A])(f: A => B): Seq[B] = seq.foldRight(Seq.empty[B]) { (a, b) => f(a) +: b }

  def foldLeftWithMutability[A, B](seq: Seq[A], identity: B)(f: (B, A) => B): B = {
    var result = identity
    seq.foreach { elt => result = f(result, elt) }
    result
  }

  //Much slower..
  def foldLeft[A, B](seq: Seq[A], id: B)(f: (B, A) => B): B = {
    def go(seq: Seq[A], b: B): B = if (seq.isEmpty) b else go(seq.tail, f(b, seq.head))

    go(seq, id)
  }

}