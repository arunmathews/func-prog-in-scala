package scala.chap8

import scala.chap8.ChapterSamples.Prop._
import scala.chap6.StatePattern.State
import scala.chap6.SamplesExercises.RNG
import scala.chap5.ChapterSamples.Stream
import scala.language.implicitConversions
import java.util.concurrent.Executors
import scala.chap7.SampleExercises.Par

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

    def **[B](gen: Gen[B]): Gen[(A, B)] = this.map2(gen)((_, _))

    //Ex 10
    def unsized: SGen[A] = SGen(_ => this)
  }

  case class SGen[+A](forSize: Int => Gen[A]) {
    //Ex 11
    def apply(n: Int): Gen[A] = forSize(n)

    def map[B](f: A => B): SGen[B] = SGen(x => forSize(x).map(f))

    def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(x => forSize(x).flatMap(f))
  }

  trait Cogen[-A] {
    def sample[B](a: A, s: State[RNG, B]): State[RNG, B]
  }

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

    //Nice - State.sequence uses map2 which will thread the state through. g.sample is a function
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

    //EX 12
    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n, g))

    //Ex 14
    def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n max 1, g))

    implicit def unsized[A](g: Gen[A]): SGen[A] = g.unsized

    //Play around with combinators
    def genTupleInt(start: Int, stopExclusive: Int): Gen[(Int, Int)] = for {
      a <- choose(start, stopExclusive)
      b <- choose(start, stopExclusive)
    } yield (a, b)

    def genOption[A](gen: Gen[A]): Gen[Option[A]] =
      boolean.flatMap(b => if (b) gen.map(Option(_)) else unit(None: Option[A]))

    def genAFromOption[A](gen: Gen[Option[A]]): Gen[A] = gen.map({
      case Some(a) => a
      //Is this the right thing to do ? Probably not.
      case None => throw new RuntimeException("Invalid option")
    })

    //Nice way of creating ASCII string
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

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

  def fn[A,B](in: Cogen[A])(out: Gen[B]): Gen[A => B] = {
    val test = (sa: State[RNG, A]) => sa.flatMap(a => in.sample(a, out.sample))

    val t2 = (ga: Gen[A]) => ga.map(a => in.sample(a, out.sample).run)

    //out.map(b => ((a: A) => in.sample(a, State.unit(b).run)))

  }
  //def listOf[A](a: Gen[A]): Gen[List[A]] - should size be specified ?
  //def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]
  //def forAll[A](a: Gen[A])(f: A => Boolean): Prop
  object Prop {
    type FailedCase = String
    type SuccessCount = Int
    type TestCases = Int
    type Result = Option[(FailedCase, SuccessCount)]
    type MaxSize = Int

    def apply(f: (TestCases, RNG) => Result): Prop = Prop {
      (_, n, rng) => f(n, rng)
    }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

    def createMessage[A](a: A, e: Exception): String =
      s"test case: $a\n" +
      s"generated an exception: ${e.getMessage}" +
      s"stack trace: \n ${e.getStackTrace}"

    def forAll[A](genA: Gen[A])(f: A => Boolean): Prop = Prop {
      (n, rng) =>
        randomStream(genA)(rng).zip(Stream.from(0)).take(n).foldRight(None: Result) {
          case ((a, i), r) => try {
            if (f(a)) r else Some(a.toString, i)
          }
          catch {
            case e: Exception => Some(createMessage(a, e), i)
          }
      }
    }

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (max, n, rng) =>
        val casesPerSize = (n + (max - 1))/max
        val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
        val prop: Prop = props.map(p => Prop {
          (max, _, rng) => p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)

        prop.run(max, n, rng)
    }

    val execGen = Gen.weighted(Gen.choose(1, 4).map(i => Executors.newFixedThreadPool(i)) -> 0.75,
      Gen.unit(Executors.newCachedThreadPool) -> 0.25)

    def forAllPar[A](genA: Gen[A])(f: A => Par[Boolean]): Prop =
      forAll(execGen ** genA) { case s ** a => f(a)(s).get() }

    def forAllPar[A](g: SGen[A])(f: A => Par[Boolean]): Prop = forAll(execGen ** g(_)) { case s ** a => f(a)(s).get() }

    def check(p: => Boolean): Prop = {
      lazy val result = p
      forAll(Gen.unit(())) { _ => result}
    }

    def checkPar(p: => Par[Boolean]): Prop = forAllPar(Gen.unit(()))(_ => p)

    def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.simple()) {
      p.run(maxSize, testCases, rng) match {
        case Some((msg, n)) => println(s" Failed after $n passed tests:\n $msg")
        case None => println(s"All tests passed. Count: $testCases")
      }
    }
  }

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    //Ex 3
    //def check: Boolean
    //def &&(p: Prop): Prop = new Prop { def check: Boolean = Prop.this.check || p.check

    //Ex 9
    def &&(p: Prop): Prop = Prop {
      (max, n, rng) => run(max, n, rng) orElse p.run(max, n, rng)
    }

    //|| means either could succeed for the prop to succeed. None is success
    def ||(p: Prop): Prop = Prop {
      (max, n, rng) => run(max, n, rng).flatMap {
        case (msg, _) => p.tag(msg).run(max, n, rng)
      }
    }

    def tag(msg: String) = Prop {
      (max, n, rng) => run(max, n, rng) map {
        case (e, c) => (msg + "\n" + e, c)
      }
    }
  }
}
