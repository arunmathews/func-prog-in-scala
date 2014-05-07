import scala.chap11Monads.SourceExercises.Monad._
import scala.chap6.SamplesExercises.{Simple, RNG}
import scala.chap6.StatePattern.State
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
val sList = List("asd", "ghfi", "abcde")
val f: A => List[B] = a => List(a.size)
val g: B => List[C] = b => List(b.toDouble)
val h: C => List[D] = c => List(c.toString)
listMonad.flatMap(listMonad.flatMap(listMonad.flatMap(sList)(f))(g))(h)
listMonad.flatMap(listMonad.flatMap(sList)(f))(b => listMonad.flatMap(g(b))(h))
listMonad.compose(listMonad.compose(f, g), h)("abcd")
listMonad.compose(f, listMonad.compose(g, h))("abcd")
val aString = "Hello, "
val bString = "Monad!"
Id(aString) flatMap (a => Id(bString) flatMap (b => Id(a + b)))
for {
  a <- Id(aString)
  b <- Id(bString)
} yield a + b

//Ex 19
type RandState[A] = State[RNG, A]
type RndStateInt = RandState[Int]
val rngStateMonad = stateMonad[RNG]
val rng = Simple(42)
val intState: RndStateInt = State(RNG.positiveLessThan(12))
val listStates = rngStateMonad.replicateM(5, intState)
val (ints, newRng) = listStates.run(rng)

RNG.positiveInt(rng)
val sri1: RandState[Int] = State(RNG.positiveLessThan(10))
val sri2: RandState[Int] = State(RNG.positiveEven)
val su1: RandState[Int] = State.unit(3)
val sl = List(sri1, sri2, su1)
val sSeq = rngStateMonad.sequence(sl)
sSeq.run(newRng)
val mappedRndInt = rngStateMonad.map2(listStates, sSeq)(_ ++ _)
mappedRndInt.run(rng)

//Ex 20
val su2 = State.unit(2)
val su3 = State.unit(1)
val s1 = State.get[RndStateInt].flatMap(s => State.set(s))
val s2 = State.set(intState).flatMap(_ => State.get)
val s2A = State.unit(intState)
val s1A: RndStateInt = State.unit(3)
s1.run(intState)._2.run(rng) == intState.run(rng)
s1A.run(rng)
val sm = State.modify[Int](s => s)
sm.run(2)
def setGet[S](s: S): State[S, S] = for {
  _ <- State.set(s)
  x <- State.get
} yield x
setGet(2).run(1)
