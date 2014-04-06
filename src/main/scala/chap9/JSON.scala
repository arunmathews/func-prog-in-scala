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

  def jsonParser[ParseError, Parser[+_]](P: Parsers[ParseError, Parser]): Parser[JSON] = {
    import P.{string => _, _}
    implicit def tok(s: String): Parser[String] = token(P.string(s))

    def array = surround("[", "]")(
      value sep "," map(vs => JArray(vs.toIndexedSeq))) scope "array"

    def value: Parser[JSON] = array
  }
}
