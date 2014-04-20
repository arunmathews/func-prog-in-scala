import scala.chap10.ChapterExercises._
import scala.chap10.ChapterExtra._
import scala.chap8.ChapterSamples.{Prop, Gen}
import scala.chap10.WC
listMonoid.zero
listMonoid.op(listMonoid.zero, List(1,3,3))
listMonoid.op(listMonoid.zero, listMonoid.zero)
val tpia = monoidLaws(intAddition, Gen.choose(1, 6))
val tps = monoidLaws(stringMonoid, Gen.stringN(8))
Prop.run(tpia)

Prop.run(tps)

val intList = List(1,2,3)
val stringList = List("  abcde ", "   de ", "    ghi   ")
def id[A]= (x: A) => x
def length[A] = listMonoidLift((x: A) => 1, intAddition)
val sum = listMonoidLift(id[Int], intAddition)
length(intList)

sum(intList)

val strLength = listMonoidLift((x: String) => x.size, intAddition)

strLength(stringList)

stringList.foldLeft(trimMonoid.zero)(trimMonoid.op)

//Ex 10

val test = IndexedSeq(1, 2, 5)

increasing(test)

ordered(test)

ordered(IndexedSeq(5, 0, -4))

ordered(IndexedSeq(5, 0, 4, -1))

val str = "  lorem ipsum dolor    sit amet,    ok "

WC.wordCount(str)

WC.wordCountflv(str)