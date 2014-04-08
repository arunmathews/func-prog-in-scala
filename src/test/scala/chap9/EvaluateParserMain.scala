package scala.chap9

import scala.chap9.ChapterExercisesSource.Location


/**
 *
 */
object EvaluateParserMain extends App {
  val jp = JSON.jsonParser(ParserImpl)

  val l1 = Location("""{"price":"as"}""")
  val l2 = Location("""{}""")
  val result = jp(l2)

  println(result)
}
