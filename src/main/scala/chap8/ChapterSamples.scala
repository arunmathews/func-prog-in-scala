package scala.chap8

import scala.chap8.ChapterSamples.Prop._
import scala.chap6.StatePattern.State
import scala.chap6.SamplesExercises.RNG
import scala.chap5.ChapterSamples.Stream
import scala.Some

/**
 * Samples and exercises from chapter 8 - Property based testing
 */
object ChapterSamples {
  //Ex 1: Sum (x n times) = n * x. Sum (all zeroes) = 0. Addition is associative. Sum of 1 to n - n * (n=1)/2
  //Ex 2: Max (List[Int]): Max (x, n times) = x, Max(all 0s) = 0, Max(all -ve) less than 0, Max empty list ?

  case class Gen[+A](sample: State[RNG,A]) {
    def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

    //Ex 6
    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

    def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = this.flatMap(a => g.map(b => f(a,b)))

    def listOfN(n: Int): Gen[List[A]] = Gen.listOfN(n, this)

    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => this.listOfN(n))
  }

  case class SGen[+A](forSize: Int => Gen[A])

  object Gen {
    //Ex 4 My solution
    def chooseExplicit(start: Int, stopExclusive: Int): Gen[Int] = {
      val sample: State[RNG, Int] = State(s => {
        val (num, rnd) = s.nextInt

        (start + (math.abs(num) % (stopExclusive - start)), rnd)
      })

      Gen(sample)
    }

    //Ex 4 better solution
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(RNG.positiveInt).map(n => start + n % (stopExclusive - start)))

    //Ex 5
    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

    //Nice - sequence uses map2 which will thread the state through
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

    //Play around with combinators
    def genTupleInt(start: Int, stopExclusive: Int): Gen[(Int, Int)] = for {
      a <- choose(start, stopExclusive)
      b <- choose(start, stopExclusive)
    } yield (a, b)

    def genOption[A](gen: Gen[A]): Gen[Option[A]] = gen.map(Option(_))

    def genAFromOption[A](gen: Gen[Option[A]]): Gen[A] = gen.map({
      case Some(a) => a
      //Is this the right thing to do ?
      case None => throw new RuntimeException("Invalid option")
    })

    //Nice way of ASCII creating string
    def stringN(n: Int): Gen[String] = listOfN(n, choose(0, 127)).map(_.map(_.toChar).mkString)

    //Ex 6
    def even(start: Int, stopExclusive: Int): Gen[Int] =
      choose(start, if (stopExclusive % 2 == 0) stopExclusive - 1 else stopExclusive).
        map(n => if (n % 2 == 0) n else n + 1)

    def odd(start: Int, stopExclusive: Int): Gen[Int] =
      choose(start, if (stopExclusive % 2 != 0) stopExclusive - 1 else stopExclusive).
        map(n => if (n % 2 != 0) n else n + 1)

    def sameParity(from: Int, to: Int): Gen[(Int,Int)] = for {
      i <- choose(from, to)
      j <- if (i % 2 == 0) even(from, to) else odd(from, to)
    } yield (i, j)

    //Ex 7
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

    //Ex 8
    def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
      val g1End = g1._2.abs / (g1._2.abs + g2._2.abs)

      Gen(State(RNG.double).flatMap(d => if (d < g1End) g1._1.sample else g2._1.sample))
    }
  }

  //def listOf[A](a: Gen[A]): Gen[List[A]] - should size be specified ?
  //def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]
  //def forAll[A](a: Gen[A])(f: A => Boolean): Prop
  object Prop {
    type FailedCase = String
    type SuccessCount = Int
    type TestCases = Int
    type Result = Option[(FailedCase, SuccessCount)]

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

    def createMessage[A](a: A, e: Exception): String =
      s"test case: $a\n" +
      s"generated an exception: ${e.getMessage}" +
      s"stack trace: \n ${e.getStackTrace}"

    def forAll[A](genA: Gen[A])(f: A => Boolean): Prop = Prop {
      (n, rng) => randomStream(genA)(rng).zip(Stream.from(0)).take(n).foldRight(None: Result) {
        case ((a, i), r) => try {
          if (f(a)) r else Some(a.toString, i)
        }
        catch {
          case e: Exception => Some(createMessage(a, e), i)
        }
      }
    }
  }

  case class Prop(run: (TestCases, RNG) => Result) {
    //Ex 3
    //def check: Boolean
    //def &&(p: Prop): Prop = new Prop { def check: Boolean = Prop.this.check || p.check

    //Ex 9
    def &&(p: Prop): Prop = Prop {
      (n, rng) => run(n, rng) orElse p.run(n, rng)
    }

    //Nice way of doing this
    def ||(p: Prop): Prop = Prop {
      (n, rng) => run(n, rng).flatMap {
        case (msg, _) => p.tag(msg).run(n, rng)
      }
    }

    def tag(msg: String) = Prop {
      (n, rng) => run(n, rng) map {
        case (e, c) => (msg + "\n" + e, c)
      }
    }
  }
}
