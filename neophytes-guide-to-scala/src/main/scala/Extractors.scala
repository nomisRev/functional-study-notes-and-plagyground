object NameExtractor {

  //  def unapplySeq(arg: String): Option[Seq[String]] = {
  //    val names = arg.split(" ")
  //    if(names.isEmpty) None else Some(names)
  //  }

  def unapplySeq(arg: String): Option[(String, String, Seq[String])] = {
    val names = arg.split(" ")
    if (names.length < 2) None else Some(names.head, names.last, names.drop(1).dropRight(1))
  }

}
