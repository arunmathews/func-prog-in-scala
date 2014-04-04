package scala.chap9

import scala.chap9.ChapterExercisesSource.Parsers

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

  /* def jsonParser[ParseError, Parser](P: Parsers[ParseError, Parser]): Parser[JSON] = {
    import P._

    val spaces = char(' ').many.slice
  } */
}
