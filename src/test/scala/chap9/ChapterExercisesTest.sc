import scala.chap9.ChapterExercisesSource.Location
import scala.chap9.{ParserImpl, JSON}

/**
 * Test code for chapter 9
 */

val ls1 = Location("9")

val p = ParserImpl.regex("\\d".r)
val js = p(ls1)
val ip = ParserImpl.map(p)(s => s.toInt)
val ji = ip(ls1)
//Error
ip(Location("a"))
val sip = ParserImpl.slice(ip)
sip(Location("129cd", 2))
val t1 = ParserImpl.string("12")
val t2 = ParserImpl.string("3")
t2(Location("123", 2))
val t3 = ParserImpl.flatMap(t1)(_ => t2)
t3(Location("123"))
val s1 = ParserImpl.string("{")
val s2 = ParserImpl.string("}")
val sp = ParserImpl.surround(s1, s2)(t2)
sp(Location("{ 3 }"))

val jp = JSON.jsonParser(ParserImpl)
jp(Location("{ }"))

