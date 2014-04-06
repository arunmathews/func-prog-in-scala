package scala.chap9

import scala.language.implicitConversions
import scala.language.higherKinds
import scala.chap9.ChapterExercisesSource._

/**
 * Create JSON parser using the primitives and combinators already defined
 */
trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P.{string => _, _}
    implicit def tok(s: String): Parser[String] = token(P.string(s))

    def array = surround("[", "]")(
      value sep "," map(vs => JArray(vs.toIndexedSeq))) scope "array"

    def keyval = escapedQuoted ** (":" *> value)
    def obj = surround("{", "}")(
      keyval sep "," map(kvs => JObject(kvs.toMap))) scope "object"

    def lit = scope("literal") {
      "null".as(JNull) |
      double.map(JNumber) |
      escapedQuoted.map(JString) |
      "true".as(JBool(get = true)) |
      "false".as(JBool(get = false))
    }

    def value: Parser[JSON] = lit | obj | array

    root(obj | array)
  }
}
