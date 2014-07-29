package scala.chap10MonoidsRev

import scala.chap8.ChapterSamples.{Gen, Prop}
import scala.chap7.SampleExercises._

/**
 * Source code plus exercises from chapter 10 - Monoids. Monoid - category with one object
 */
object SourcePlusExercises {
  trait Monoid[A] {
    def op(a1: A, a2: A): A

    def zero: A
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)

    override def zero: A = m.zero
  }

  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override val zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero: List[A] = Nil
  }

  //Ex 1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  //Ex 2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None
  }

  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  //Ex 3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 andThen a2

    override def zero: (A) => A = (a:A) => a
  }

  //Ex 4
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = Prop.forAll(for {
    x <- gen
    y <- gen
    z <- gen
  } yield (x, y, z))(p => m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)) &&
  Prop.forAll(gen)((a: A) => m.op(a, m.zero) == a && m.op(a, m.zero) == a)

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  //Ex 5
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  //Ex 6
  def foldRightFoldMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, dual(endoMonoid[B]))(f.curried)(z)
  //Ex 6
  def foldLeftFoldMap[A, B](as: List[A])(z: B)(f: (B, A) => B): B = foldMap(as, endoMonoid[B])(a => b => f(b ,a))(z)

  //Ex 7
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    v.length match {
      case i: Int if i==0 => m.zero
      case i: Int if i==1 => f(v(0))
      case _ =>
        val (left, right) = v.splitAt(v.length/2)
        m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }

  //Ex 8
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(para1: Par[A], para2: Par[A]): Par[A] = para1.map2(para2)(m.op)

    override def zero: Par[A] = Par.unit(m.zero)
  }
  //Incomplete - only reducing in parallel not mapping
  def parFoldMapIncomplete[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = foldMapV(v, par(m))(Par.asyncF(f))
  //Nice. Map in parallel first and then use par function above to
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v.toList)(f).flatMap {
      seqB => foldMapV(seqB.toIndexedSeq, par(m))(b => Par.lazyUnit(b))
    }

  //Ex 9
  def increasing(ints: IndexedSeq[Int]): Boolean = {
    val increasingMonoid = new Monoid[(Boolean, Int)] {
      override def op(a1: (Boolean, Int), a2: (Boolean, Int)): (Boolean, Int) = (a1._1 && a2._1 && a1._2 < a2._2, a2._2)

      override def zero: (Boolean, Int) = (true, Int.MinValue)
    }

    foldMapV(ints, increasingMonoid)(x => (true, x))._1
  }

}
