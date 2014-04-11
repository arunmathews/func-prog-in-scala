import scala.chap9.ChapterExercisesSource.Location
import scala.chap9.{ParserImpl, JSON}

/**
 * Test code for chapter 9
 */

val ls1 = Location("ab9", 2)

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
val t4 = ParserImpl.map2(t1, t2)(_.toInt + _.toInt)
t4(Location("123"))
val t5 = ParserImpl.skipL(t1, t2)
t5(Location("123"))
val t6 = ParserImpl.skipR(t1, t2)
t6(Location("123"))
val s1 = ParserImpl.string("{")
val s2 = ParserImpl.string("}")
val sp = ParserImpl.surround(ParserImpl.token(s1), s2)(ParserImpl.token(t2))
sp(Location("{ 3 }"))
val tlp = ParserImpl.token(t2)
tlp(Location("3"))
tlp(Location("3  "))
val at2 = ParserImpl.attempt(t2)
val sr = ParserImpl.skipR(t2, ParserImpl.whiteSpace)
sr(Location("3  "))
val slp = ParserImpl.skipL(ParserImpl.token(s1), ParserImpl.token(t2))
slp(Location("{ 3"))
val llp = ParserImpl.many(t2)
llp(Location("3333"))
val listLp = ParserImpl.listOfN(3, t2)
listLp(Location("333"))
val pp = ParserImpl.product(t1, t2)
pp(Location("123"))
val colP = ParserImpl.string(":")
val esp = ParserImpl.quotedComplex
val valeP = ParserImpl.skipL(colP, esp)
val cesp = ParserImpl.product(esp, valeP)
cesp(Location("\"12\":\"34\""))
val qp = ParserImpl.quoted
val qep = ParserImpl.product(qp, colP)
qep(Location("\"12\":"))
val jp = JSON.jsonParser(ParserImpl)
jp(Location("""{"a":1.1}"""))
jp(Location("""{"a":"b"}"""))

