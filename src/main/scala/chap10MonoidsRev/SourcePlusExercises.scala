package scala.chap10MonoidsRev

import scala.chap8.ChapterSamples.{Gen, Prop}
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
}
