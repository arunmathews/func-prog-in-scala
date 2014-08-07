import scala.chap11MonadsRev.SourcePlusExercises.Monad._

listMonad.sequence(List(List(1,2, 6), List(3, 4)))

for {
  x <- List(1, 2, 6)
  y <- List(3, 4)
} yield x :: List(y)

listMonad.traverse(List(1,2,3))(List(_))

val listR = listMonad.replicateM(5, List(1, 2))

listR.size
//Flatmap and map over options. Will return None for the operation as soon as None is seen
optionMonad.traverse(List(1,2)) {
  case 1 => Some(1)
  case _ => None
}
optionMonad.sequence(List(Some(4), Some(5), None))

optionMonad.replicateM(5, Some('a'))

optionMonad.replicateM(3, None)

