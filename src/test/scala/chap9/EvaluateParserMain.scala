package scala.chap9

import scala.chap9.ChapterExercisesSource.Location


/**
 *
 */
object EvaluateParserMain extends App {
  val jp = JSON.jsonParser(ParserImpl)

  val l1 = Location("""{}""")
  val l2 = Location("""{"price":1.12}""")
  val l3 = Location("""{"price":true}""")
  val l4 = Location("""{"price":[1.1,2.0]}""")
  val l5 = Location("""{"price":"as"}""")
  val l6 = Location("""{"price":{"b":1.3}}""")

  println(jp(l1))
  println(jp(l2))
  println(jp(l3))
  println(jp(l4))
  println(jp(l5))
  println(jp(l6))
}
