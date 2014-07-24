import scala.chap10MonoidsRev.SourcePlusExercises._

listMonoid.zero

listMonoid.op(List(1,2), List(3,4))

listMonoid.op(List(2,4), listMonoid.zero)

val words = List("Dallas", "Cowboys", "Texas", "Longhorns")

words.foldLeft(stringMonoid.zero)(stringMonoid.op)

words.foldRight(stringMonoid.zero)(stringMonoid.op)

foldLeftFoldMap(words)("")(_ + _)

foldRightFoldMap(words)("")(_ + _)

foldMapV(words.toIndexedSeq, stringMonoid)((a: String) => a)
