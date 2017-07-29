package chapter6

object MapsAndSetsEx {

  val people = Set(
    "Alice",
    "Bob",
    "Charlie",
    "Derek",
    "Edith",
    "Fred")

  val ages = Map(
    "Alice" -> 20,
    "Bob" -> 30,
    "Charlie" -> 50,
    "Derek" -> 40,
    "Edith" -> 10,
    "Fred" -> 60)

  val favoriteColors = Map(
    "Bob" -> "green",
    "Derek" -> "magenta",
    "Fred" -> "yellow")

  val favoriteLolcats = Map(
    "Alice" -> "Long Cat",
    "Charlie" -> "Ceiling Cat",
    "Edith" -> "Cloud Cat")

  def favoriteColor(name: String): Option[String] = favoriteColors.get(name)

  def favoriteColorOrBeige(name: String): String = favoriteColors.getOrElse(name, "beige")

  //It is good practice to add "()" when return type is Unit to indicate it's a side-effect/method and not a property
  def printColors(): Unit = favoriteColors.foreach(a => println(a._2))

  def lookup[A](key: String, map: Map[String, A]): Option[A] = map.get(key)

  val oldest: Option[String] =
    people.foldLeft(Option.empty[String]) { (older, person) =>
      if (ages.getOrElse(person, 0) > older.flatMap(ages.get).getOrElse(0)) {
        Some(person)
      } else {
        older
      }
    }

  val favorite: Option[String] = oldest.flatMap(a => favoriteColor(a))

  def union[A](set1: Set[A], set2: Set[A]): Set[A] = set1.foldLeft(set2) { (set, a) => set + a }

  //  def union[A,B](map1: Map[A,B], map2: Map[A,B])(f: (B,B) => B):  Map[A,B] = map1.foldLeft(map2){ (map, b) =>
  //    if(map.contains(b._1)) map.updated(b._1, map.get)
  //  }

  def union[A](map1: Map[A, Int], map2: Map[A, Int]): Map[A, Int] = map1.foldLeft(map2) { (map, b) =>
    val (k, v) = b
    val newValue = map.get(k).map(a => a + v).getOrElse(v)
    map.updated(k, newValue)
  }

  def unionGeneric[A, B](map1: Map[A, B], map2: Map[A, B])(f: (B, B) => B): Map[A, B] = map1.foldLeft(map2) { (map, b) =>
    val (k, v) = b
    val newValue = map.get(k).map(a => f(a, v)).getOrElse(v)
    map.updated(k, newValue)
  }



}
