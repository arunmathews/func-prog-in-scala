package scala.chap8

import scala.chap8.ChapterSamples.Gen
import scala.chap6.SamplesExercises.{Simple, RNG}
import scala.chap6.StatePattern.State

/**
 *
 */
object EvaluateMain extends App {
  val genInt = Gen.choose(1, 6)
  val intGen = genInt.sample

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

  val rng = Simple(42)

  val (test, finalRng) = stringToIntGen.sample.run(rng)

  println(test("abc"))
  println(test("def"))
  println(test("we are we are"))
}
