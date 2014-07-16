import scala.chap10MonoidsRev.SourcePlusExercises._

listMonoid.zero

listMonoid.op(List(1,2), List(3,4))

listMonoid.op(List(2,4), listMonoid.zero)