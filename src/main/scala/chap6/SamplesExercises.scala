package scala.chap6

import scala.annotation.tailrec

/**
 * Samples and exercises from chapter 6
 */
object SamplesExercises {

  type Rand[+A] = RNG => (A, RNG)

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt

      (n, nextRNG)
    }
  }

  object RNG {
    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)

        (f(a), rng2)
      }

    //Ex 6
    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      rng => {
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)

        (f(a, b), rng3)
      }

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

    def randomPair(rng: RNG): ((Int, Int), RNG) = {
      val (n1, rng2) = rng.nextInt
      val (n2, rng3) = rng2.nextInt

      ((n1, n2), rng3)
    }

    def boolean(rng: RNG): (Boolean, RNG) =
      rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

    //Ex1
    def positiveInt(rng: RNG): (Int, RNG) = {
      val (n, rng1) = rng.nextInt

      ( if ( n < 0) -(n+1) else n, rng1)
    }

    def positiveEven: Rand[Int] = map(positiveInt)(i => i - i % 2)

    //Ex 2
    def doubleNaive(rng: RNG): (Double, RNG)  = {
      val (n, rng1) = positiveInt(rng)

      (n/(Int.MaxValue.toDouble+1), rng1)
    }

    //Ex 5
    def double: Rand[Double] = map(positiveInt)(_/(Int.MaxValue.toDouble+1))

    //Ex 3
    def intDouble(rng: RNG): ((Int,Double), RNG) = {
      val (i, rng1) = positiveInt(rng)
      val (d, rng2) = double(rng1)

      ((i, d), rng2)
    }

    def doubleInt(rng: RNG): ((Double,Int), RNG) = {
      val ((i, d), rng1) = intDouble(rng)

      ((d, i), rng1)
    }

    def double3(rng: RNG): ((Double,Double,Double), RNG) = {
      val (d1, r1) = double(rng)
      val (d2, r2) = double(r1)
      val (d3, r3) = double(r2)

      ((d1, d2, d3), r3)
    }

    //Ex 4
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

      @tailrec
      def acc(list: List[Int], subRng: RNG, n: Int): (List[Int], RNG) = {
        if(n==0) (list, subRng)
        else {
          val (newInt, newRng) = subRng.nextInt
          acc(newInt::list, newRng, n-1)
        }
      }

      acc(List[Int](), rng, count)
    }

    //Ex 7
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      fs.foldRight(unit(List[A]()))((newR, rl) => map2(newR, rl)((a, b) => a::b))

    def positiveLessThanBad(n: Int): Rand[Int] = map(positiveInt){ _ % n}

    //Ex 8
    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, rng1) = f(rng)

        g(a)(rng1)
      }

    def positiveLessThan(n: Int): Rand[Int] =
      flatMap(positiveInt) {
        i =>
          val mod = i % n
          if (i - mod + (n-1) > 0) unit(mod) else positiveLessThan(n)

      }

    //Ex 9
    def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

    //Nice - how to use map and flatMap together
    def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra)(a => mapWithFlatMap(rb)(b => f(a, b)))
  }
}
