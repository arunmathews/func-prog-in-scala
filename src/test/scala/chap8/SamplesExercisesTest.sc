import scala.chap6.SamplesExercises.Simple
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

val forallMax = createProp(smallInt, Gen.listOf)

//Fails when testing empty list
Prop.run(forallMax)




val forAllMaxAtleast1 = createProp(smallInt, Gen.listOf1)
Prop.run(forAllMaxAtleast1)

//Ex 15. Testing using zip and tail comparison

val sortedProp = Prop.forAll(Gen.listOf(smallInt)) { l =>
  val ls = l.sorted
  l.isEmpty || ls.tail.isEmpty || ls.zip(ls.tail).forall { case (a, b) => a <= b}
}

Prop.run(sortedProp)


