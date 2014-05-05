import scala.chap11Monads.SourceExercises.Monad._

val op: Option[Int] = None
val op2: Option[Int] = Some(5)
optionMonad.replicateM(5, op)
optionMonad.replicateM(4, op2)
optionMonad.sequence(List(op, op2, Some(7)))
val al = List(1, 2, 3, 4)
optionMonad.filterM(al)((a: Int) => if (a < 3) Option(a%2  == 0) else None)
listMonad.filterM(al)((a: Int) => List(a%2  != 0))

val lj = listMonad.join(List(List(1, 2), List(3, 4)))

val l3l: List[List[List[Int]]] =
  List(List(List(1,2), List(3,4)),
    List(List(), List(5)))

val l3lf = l3l.flatten
val l2l = listMonad.map(l3l)(listMonad.join)
listMonad.join(l3lf)
listMonad.join(l2l)
type A = String
type B = Int
type C = Double
type D = String
val sl = List("asd", "ghfi", "abcde")
val f: A => List[B] = a => List(a.size)
val g: B => List[C] = b => List(b.toDouble)
val h: C => List[D] = c => List(c.toString)
listMonad.flatMap(listMonad.flatMap(listMonad.flatMap(sl)(f))(g))(h)

listMonad.flatMap(listMonad.flatMap(sl)(f))(b => listMonad.flatMap(g(b))(h))

listMonad.compose(listMonad.compose(f, g), h)("abcd")
listMonad.compose(f, listMonad.compose(g, h))("abcd")

val aString = "Hello, "
val bString = "Monad!"
Id(aString) flatMap (a => Id(bString) flatMap (b => Id(a + b)))

for {
  a <- Id(aString)
  b <- Id(bString)
} yield a + b

