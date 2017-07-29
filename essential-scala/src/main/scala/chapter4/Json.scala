package chapter4

/**
  * Json ::= JsNumber value:Double
  * | JsString value:String
  * | JsBoolean value:Boolean
  * | JsNull
  * | JsSequence
  * | JsObject
  *
  * JsSequence ::= SeqCell head:Json tail:JsSequence
  * | SeqEnd
  *
  * JsObject ::= ObjectCell key:String value:Json tail:JsObject
  * | ObjectEnd
  *
  */
sealed trait Json {
  def print: String = {
    def seqToJson(seq: SeqCell): String = seq match {
        //Prepending s to any string literal allows the usage of variables directly in the string.
      case SeqCell(h, t@SeqCell(_, _)) => s"${h.print}, ${seqToJson(t)}"
      case SeqCell(h, SeqEnd) => h.print

    }

    def objToJson(obj: JsObject): String = obj match {
      case ObjectCell(k, v, ObjectEnd) => s"\042$k\042: ${v.print}"
      case ObjectCell(k,v,t) =>  s"\042$k\042: ${v.print}, ${objToJson(t)}"
      case ObjectEnd => "" // this will never occur because we check for ObjectEnd in the first case
    }

    this match {
      case JsNumber(v) => v.toString
      case JsString(v) => "'" + v + "'"
      case JsBoolean(v) => v.toString
      case JsNull => "null"
      case s@SeqCell(_,_) => s"[ ${seqToJson(s)} ]"
      case SeqEnd => "[]"
      case o@ObjectCell(_,_,_) => s"{ ${objToJson(o)} }"
      case ObjectEnd => "{}"
    }
  }
}

final case class JsNumber(value: Double) extends Json
final case class JsString(value: String) extends Json
final case class JsBoolean(value: Boolean) extends Json
case object JsNull extends Json
sealed trait JsSequence extends Json
final case class SeqCell(head: Json, tail: JsSequence) extends JsSequence
case object SeqEnd extends JsSequence

sealed trait JsObject extends Json
final case class ObjectCell(key: String, value: Json, tail: JsObject) extends JsObject
case object ObjectEnd extends JsObject