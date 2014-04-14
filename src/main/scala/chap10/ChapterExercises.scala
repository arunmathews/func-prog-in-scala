package scala.chap10

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
}
