import java.util.concurrent.{Executors, ExecutorService}
import scala.chap6.SamplesExercises.{RNG, Simple}
import scala.chap6.StatePattern.State
import scala.chap7.SampleExercises._
import scala.chap8.ChapterSamples._
/**
 * Test samples, exercises from chapter 8 - Property base testing
 */
val genInt = Gen.choose(1, 6)
val intGen = genInt.sample

val rng = Simple(42)
val (newInt, newRng) = intGen.run(rng)


val (nextInt, nextRng) = intGen.run(newRng)



val unitGen = Gen.unit('a)

unitGen.sample.run(rng)

unitGen.sample.run(newRng)

val forAllRemainderProp = Prop.forAll(genInt)(a => a%2==0)

forAllRemainderProp.run(6, 2, rng)
Prop.run(forAllRemainderProp, 6, 2)




val createProp = (g: Gen[Int], f: Gen[Int] => SGen[List[Int]]) =>
  Prop.forAll(f(g)) { l =>
    val max = l.max
    !l.exists(_ > max)
  }

val smallInt = Gen.choose(-10, 10)

//Fails when testing empty list
Prop.run(createProp(smallInt, Gen.listOf))








Prop.run(createProp(smallInt, Gen.listOf1))


//Ex 15. Testing using zip and tail comparison
val sortedProp = Prop.forAll(Gen.listOf(smallInt)) { l =>
  val ls = l.sorted
  l.isEmpty || ls.tail.isEmpty || ls.zip(ls.tail).forall { case (a, b) => a <= b}
}

Prop.run(sortedProp)

val ES: ExecutorService = Executors.newCachedThreadPool

val parCheck = Prop.check {
  Par.equal(Par.unit(1).map(_ + 1), Par.unit(2)) (ES).get()
}

val parCheck2 = Prop.checkPar {
  Par.equal(Par.unit(1).map(_ + 1), Par.unit(2))
}
Prop.run(parCheck)

Prop.run(parCheck2)


val pint = Gen.choose(0, 10) map (Par.unit(_))
val p4 = Prop.forAllPar(pint)(n => Par.equal(n.map(y => y), n))
Prop.run(p4)

//Ex 16
val pint2 = Gen.choose(-100, 100).listOfN(Gen.choose(0, 20)).map(
  l => l.foldLeft(Par.unit(0))((p, i) => Par.fork {
    p.map2(Par.unit(i))(_ + _)
  }))

val pint2Prop = Prop.forAllPar(pint2)(n => Par.equal(n.map(y => y), n))

//Prop.run(pint2Prop)

//Ex 17
val pfork = Prop.forAllPar(pint2)(n => Par.equal(Par.fork(n), n))

//Ex 18
//Length should be <=
//take while + dropwhile == l

//Ex 19: My solution for Gen[String => Int] inspired from solution for general case. Is this right ?
def genStringIntFn(g: Gen[Int]): Gen[String => Int] = {
  val rngState = (rng: RNG) => ((s: String) => {
    val hash = s.hashCode
    val srng = Simple(hash)
    val (num, _) = g.sample.run(srng)

    num
  }, rng)

  Gen(State(rngState))
}

val stringToIntGen = genStringIntFn(genInt)

val (test, finalRng) = stringToIntGen.sample.run(nextRng)

ES.shutdown()

