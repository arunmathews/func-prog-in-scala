package scala.chap10

import scala.chap8.ChapterSamples.{Gen, Prop}
import scala.chap7.SampleExercises._

/**
 * Code from chap10 - both source and exercises
 */
object ChapterExercises {
  trait Monoid[A] {
    def op(a1: A, a2: A): A

    def zero: A
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def zero: A = m.zero

    override def op(a1: A, a2: A): A = m.op(a2, a1)
  }

  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def zero: List[A] = Nil

    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  }

  //Ex 1
  val intAddition = new Monoid[Int] {
    override def zero: Int = 0

    override def op(a1: Int, a2: Int): Int = a1 + a2
  }

  val intMultiplication = new Monoid[Int] {
    override def zero: Int = 1

    override def op(a1: Int, a2: Int): Int = a1 * a2
  }

  val booleanOr = new Monoid[Boolean] {
    override def zero: Boolean = false

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  }

  val booleanAnd = new Monoid[Boolean] {
    override def zero: Boolean = true

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  }

  //Ex 2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def zero: Option[A] = None

    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
  }

  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid
  def secondOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def zero: (A) => A = (a:A) => a

    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 andThen a2
  }

  //Ex 4
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = Prop.forAll(for {
    x <- gen
    y <- gen
    z <- gen
  } yield (x, y, z))(p =>
    m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)) &&
    Prop.forAll(gen)(a => m.op(a, m.zero) == a && m.op(m.zero, a) == a)

  //Ex 5
  def trimMonoid: Monoid[String] = new Monoid[String] {
    override def zero: String = ""

    override def op(a1: String, a2: String): String = (a1.trim + " " + a2.trim).trim
  }

  //Ex 6
  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  //Ex 7
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  //Ex 8
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)
  
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    //Reverse the order because we are going from left to right
    val em = dual(endoMonoid[B])

    foldMap(as, em)(a => b=> f(b, a))(z)
  }

  //Ex 9
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    as.size match {
      case 0 => m.zero
      case 1 => f(as(0))
      case n =>
        val (al, ar) = as.splitAt(n / 2)
        val fl = foldMapV(al, m)(f)
        val fr = foldMapV(ar, m)(f)

        m.op(fl, fr)
    }
  }

  //Got this right
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def zero: Par[A] = Par.unit(m.zero)

    override def op(pa1: Par[A], pa2: Par[A]): Par[A] = pa1.map2(pa2)(m.op)
  }

  //Incomplete - mapping is parallel. Folding is not
  def parFoldMapIncomplete[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = foldMapV(v, par(m))(Par.asyncF(f))

  //Nicely done - Mapping and folding is done in parallel
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v.toList)(f).flatMap(bs => foldMapV(bs.toIndexedSeq, par(m))(b => Par.lazyUnit(b)))

  //Ex 10. Checks only for increasing sequence
  def increasing(ints: IndexedSeq[Int]): Boolean = {
    val mon = new Monoid[(Boolean, Int)] {
      override def zero: (Boolean, Int) = (true, Int.MinValue)

      override def op(a1: (Boolean, Int), a2: (Boolean, Int)): (Boolean, Int) =
        (a1._1 && a2._1 && a1._2 < a2._2, a2._2)
    }

    foldMapV(ints, mon)(x => (true, x))._1
  }

  //Ex 10 using Option. Need to keep min and max integers. Answers are not fully correct. 
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val mon = new Monoid[Option[(Boolean, Int, Int)]] {
      override val zero: Option[(Boolean, Int, Int)] = None

      override def op(a1: Option[(Boolean, Int, Int)], a2: Option[(Boolean, Int, Int)]): Option[(Boolean, Int, Int)] =
        (a1, a2) match {
          case (Some((b1, min1, max1)), Some((b2, min2, max2))) =>
            Some((b1 && b2 && (max1 <= min2 || max2 <= min1), min1 min min2, max1 max max2))
          case (None, x) => x
          case (x, None) => x
        }
    }

    foldMapV(ints, mon)(x => Some((true, x, x))).fold(true)(_._1)
  }

  //Ex 17
  def productMonoid[A,B](a: Monoid[A], b: Monoid[B]): Monoid[(A,B)] = new Monoid[(A, B)] {
    override val zero: (A, B) = (a.zero, b.zero)

    override def op(a1: (A, B), a2: (A, B)): (A, B) = (a.op(a1._1, a2._1), b.op(a1._2, a2._2))
  }

  //Ex 18. There is no Either monoid. Zero ? How to combine either

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K,V]] = new Monoid[Map[K, V]] {
    override def zero: Map[K, V] = Map()

    override def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = a.map {
      case (k, v) => (k, V.op(v, b.getOrElse(k, V.zero)))
    } ++ b.filter(kv => !a.keySet(kv._1))
  }

  //Ex 19
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: (A) => B, a2: (A) => B): (A) => B = (a: A) => B.op(a1(a), a2(a))

    override def zero: (A) => B = (a: A) => B.zero
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val M: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
    foldMapV(as, M)((a: A) => Map(a -> 1))
  }
}
