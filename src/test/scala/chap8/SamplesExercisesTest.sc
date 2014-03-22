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

val forAllProp = Prop.forAll(genInt)(a => a%2==0)

forAllProp.run(4, rng)

