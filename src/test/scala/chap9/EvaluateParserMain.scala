package scala.chap9

import scala.chap9.ChapterExercisesSource.Location


/**
 *
 */
object EvaluateParserMain extends App {
  val jp = JSON.jsonParser(ParserImpl)

  jp(Location("""{ "price" : "as" }"""))
}
