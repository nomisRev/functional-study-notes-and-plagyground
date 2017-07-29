package red_book_exercises

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class EssentialScalaChapter4ExtendedExample extends FlatSpec with PropertyChecks {

  sealed trait Expression {
    def eval: Calculation = this match {
      case Number(v) => Success(v)
      case Addition(l, r) => l.eval match {
        case Failure(reason) => Failure(reason)
        case Success(ll) => r.eval match {
          case Failure(reason) => Failure(reason)
          case Success(rr) => Success(ll + rr)
        }
      }
      case Subtraction(l, r) => l.eval match {
        case Failure(reason) => Failure(reason)
        case Success(ll) => r.eval match {
          case Failure(reason) => Failure(reason)
          case Success(rr) => Success(ll - rr)
        }
      }
      case Division(l, r) => l.eval match {
        case Failure(reason) => Failure(reason)
        case Success(ll) => r.eval match {
          case Failure(reason) => Failure(reason)
          case Success(rr) => if (rr == 0) Failure("Division by zero") else Success(ll / rr)
        }
      }
      case SquareRoot(v) => v.eval match {
        case Failure(reason) => Failure(reason)
        case Success(vv) => if (vv < 0) Failure("Square root of negative number") else Success(Math.sqrt(vv))
      }
    }
  }

  final case class Number(value: Double) extends Expression

  final case class Addition(left: Expression, right: Expression) extends Expression

  final case class Subtraction(left: Expression, right: Expression) extends Expression

  final case class Division(left: Expression, right: Expression) extends Expression

  final case class SquareRoot(value: Expression) extends Expression


  sealed trait Calculation

  final case class Success(value: Double) extends Calculation

  final case class Failure(message: String) extends Calculation


  sealed trait Json {
    def print: String = {
      def quote(s: String): String =
        '"'.toString + s + '"'.toString

      def seqToJson(seq: SeqCell): String =
        seq match {
          case SeqCell(h, t@SeqCell(_, _)) =>
            s"${h.print}, ${seqToJson(t)}"
          case SeqCell(h, SeqEnd) => h.print
        }

      def objectToJson(obj: ObjectCell): String =
        obj match {
          case ObjectCell(k, v, t@ObjectCell(_, _, _)) =>
            s"${quote(k)}: ${v.print}, ${objectToJson(t)}"
          case ObjectCell(k, v, ObjectEnd) =>
            s"${quote(k)}: ${v.print}"
        }

      this match {
        case JsNumber(v) => v.toString
        case JsString(v) => quote(v)
        case JsBoolean(v) => v.toString
        case JsNull => "null"
        case s@SeqCell(_, _) => "[" + seqToJson(s) + "]"
        case SeqEnd => "[]"
        case o@ObjectCell(_, _, _) => "{" + objectToJson(o) + "}"
        case ObjectEnd => "{}"
      }
    }
  }

  final case class JsString(v: String) extends Json

  final case class JsNumber(v: Double) extends Json

  final case class JsBoolean(v: Boolean) extends Json

  final case object JsNull extends Json

  sealed trait JsSequence extends Json

  final case class SeqCell(head: Json, tail: JsSequence) extends JsSequence

  final case object SeqEnd extends JsSequence

  sealed trait JsObject extends Json

  final case class ObjectCell(key: String, value: Json, tail: JsObject) extends JsObject

  final case object ObjectEnd extends JsObject


  behavior of "exercise"

  it should "work" in {
    assert(Addition(SquareRoot(Number(-1.0)), Number(2.0)).eval == Failure("Square root of negative number"))
    assert(Addition(SquareRoot(Number(4.0)), Number(2.0)).eval == Success(4.0))
    assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))
  }

}