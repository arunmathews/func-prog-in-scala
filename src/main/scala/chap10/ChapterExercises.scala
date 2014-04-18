package scala.chap10

import scala.chap8.ChapterSamples.{Gen, Prop}

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

    override def op(a1: String, a2: String): String = a1.trim + " " + a2.trim
  }

  //Ex 6
  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  //Ex 7
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  //Ex 8
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    //Reverse the order because we are going from left to right
    val em = dual(endoMonoid[B])

    foldMap(as, em)(a => b=> f(b, a))(z)
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)
}
