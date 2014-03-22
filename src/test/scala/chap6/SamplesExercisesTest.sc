import scala.chap6.SamplesExercises._
import scala.chap6.StatePattern._
import scala.chap6.CandyDispenser._

val rng = Simple(42)
val (n1, rng2) = rng.nextInt




val (n2, rng3) = rng2.nextInt




val (d, rng4) = RNG.double(rng3)




val ((d1, d2, d3), rng5) = RNG.double3(rng4)










val listR = RNG.ints(4)(rng5)



val int: Rand[Int] = _.nextInt



int(listR._2)
val randIntDouble: Rand[(Int, Double)] = RNG.both(int, RNG.double)



val randDoubleInt: Rand[(Double, Int)] = RNG.both(RNG.double, int)



RNG.sequence(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))(listR._2)._1

val (i, rng6) = RNG.positiveLessThan(6)(rng5)




type RandState[A] = State[RNG, A]

val su1: RandState[Int] = State.unit(1)
val su2: RandState[Int] = State.unit(2)
val su3: RandState[Int] = State.unit(3)

val sl = List(su1, su2, su3)



val sSeq = State.sequence(sl)



sSeq.run(rng)

val machine = simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Turn, Coin, Turn))



machine.run(Machine(locked = true, 5, 10))










