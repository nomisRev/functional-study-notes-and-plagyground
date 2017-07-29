package chapter7

sealed trait JsValue {
  def stringify: String
}

final case class JsObject(values: Map[String, JsValue]) extends JsValue {
  def stringify: String = values
    .map { case (name, value) => "\"" + name + "\":" + value.stringify }
    .mkString("{", ",", "}")
}

final case class JsString(value: String) extends JsValue {
  def stringify: String = "\"" + value.replaceAll("\\|\"", "\\\\$1") + "\""
}

trait JsWriter[A] {
  def write(value: A): JsValue
}

object JsonImplicits {
  //Celebration @ my desk that I used context bound implicits but regular might be more readable
  //  def toJson[A >: JsWriter[A]](value: A): JsValue = implicitly[JsWriter[A]].write(value)

  implicit class JsUtil[A](value: A) {
    def toJson(implicit writer: JsWriter[A]): JsValue =
      writer write value
  }

  implicit object StringWriter extends JsWriter[String] {
    def write(value: String) = JsString(value)
  }

  implicit object DateWriter extends JsWriter[java.util.Date] {
    def write(value: java.util.Date) = JsString(value.toString)
  }

}

import java.util.Date

sealed trait Visitor {
  def id: String

  def createdAt: Date

  def age: Long = new Date().getTime - createdAt.getTime
}

final case class Anonymous(id: String, createdAt: Date = new Date()) extends Visitor

object Anonymous {
  implicit val jsWriter = new JsWriter[Anonymous] {

    import chapter7.JsonImplicits._

    override def write(value: Anonymous) = JsObject(
      Map(
        "id" -> value.id.toJson,
        "createdAt" -> value.createdAt.toJson
      )
    )
  }
}

final case class User(id: String, email: String, createdAt: Date = new Date()) extends Visitor

object User {
  implicit val jsWriter = new JsWriter[User] {

    import chapter7.JsonImplicits._

    override def write(value: User): JsValue = JsObject(
      Map(
        "id" -> value.id.toJson,
        "email" -> value.email.toJson,
        "createdAt" -> value.createdAt.toJson
      )
    )
  }
}


object Visitor {
  implicit object VisitorWriter extends JsWriter[Visitor] {

    import chapter7.JsonImplicits._

    def write(value: Visitor) = value match {
      case anon: Anonymous => anon.toJson
      case user: User => user.toJson
    }
  }
}
