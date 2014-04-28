import scala.chap11Monads.SourceExercises._

val opM = Monad.optionMonad
val liM = Monad.listMonad
val op: Option[Int] = None
val op2: Option[Int] = Some(5)
opM.replicateM(5, op)
opM.replicateM(4, op2)
opM.sequence(List(op, op2, Some(7)))
val al = List(1, 2, 3, 4)
opM.filterM(al)((a: Int) => if (a < 3) Option(a%2  == 0) else None)

liM.filterM(al)((a: Int) => List(a%2  != 0))