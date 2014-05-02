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

val lj = liM.join(List(List(1, 2), List(3, 4)))

val l3l: List[List[List[Int]]] =
  List(List(List(1,2), List(3,4)),
    List(List(), List(5)))

val l3lf = l3l.flatten
val l2l = liM.map(l3l)(liM.join)
liM.join(l3lf)
liM.join(l2l)

type A = String
type B = Int
type C = Double
type D = String

val sl = List("asd", "ghfi", "abcde")
val f: A => List[B] = a => List(a.size)
val g: B => List[C] = b => List(b.toDouble)
val h: C => List[D] = c => List(c.toString)

liM.flatMap(liM.flatMap(liM.flatMap(sl)(f))(g))(h)

liM.flatMap(liM.flatMap(sl)(f))(b => liM.flatMap(g(b))(h))

liM.compose(liM.compose(f, g), h)("abcd")
liM.compose(f, liM.compose(g, h))("abcd")