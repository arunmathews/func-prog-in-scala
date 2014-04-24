import scala.chap10Monoids.ChapterExercises._
import scala.chap10Monoids.ChapterExtra._
import scala.chap3.Chapter.{Leaf, Branch}
import scala.chap8.ChapterSamples.{Prop, Gen}
import scala.chap10Monoids.{ListFoldable, TreeFoldable, WC}
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
val tree = Branch(Leaf(1), Branch(Leaf(3), Leaf(5)))
TreeFoldable.toList(tree)
TreeFoldable.toListFM(tree)

val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))

val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
val m2 = Map("o1" -> Map("i2" -> 3))
val m3 = M.op(m1, m2)

bag(Vector("a", "rose", "is", "a", "rose"))

val m = productMonoid(intAddition, intAddition)

//Combine addition and length in one go
val p = ListFoldable.foldMap(intList)(a => (1, a))(m)

