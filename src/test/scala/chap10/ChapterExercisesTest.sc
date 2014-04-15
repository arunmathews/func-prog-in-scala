import scala.chap10.ChapterExercises._
import scala.chap8.ChapterSamples.{Prop, Gen}
listMonoid.zero
listMonoid.op(listMonoid.zero, List(1,3,3))
listMonoid.op(listMonoid.zero, listMonoid.zero)

val tpia = monoidLaws(intAddition, Gen.choose(1, 6))
val tps = monoidLaws(stringMonoid, Gen.stringN(8))
Prop.run(tpia)

Prop.run(tps)




