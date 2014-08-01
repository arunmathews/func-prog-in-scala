import scala.chap10MonoidsRev.SourcePlusExercises._
import scala.chap10Monoids.WC._

listMonoid.zero

listMonoid.op(List(1,2), List(3,4))

listMonoid.op(List(2,4), listMonoid.zero)

val words = List("Dallas", "Cowboys", "Texas", "Longhorns")

words.foldLeft(stringMonoid.zero)(stringMonoid.op)

words.foldRight(stringMonoid.zero)(stringMonoid.op)

foldLeftFoldMap(words)("")(_ + _)

foldRightFoldMap(words)("")(_ + _)

foldMapV(words.toIndexedSeq, stringMonoid)((a: String) => a)

increasing(IndexedSeq(1,4,7,9))

increasing(IndexedSeq(1, 3, 8, 4))

wordCount("we are we are ")

wordCountflv("we are we are ")

val M: Monoid[Map[String, Map[String, Int]]] =
  mapMergeMonoid(mapMergeMonoid(intAddition))

val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))

val m2 = Map("o1" -> Map("i2" -> 3), "o2" -> Map("i2" -> 5))

M.op(m1, m2)

bag(Vector("a", "rose", "is", "a", "rose"))