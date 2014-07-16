package scala.chap10MonoidsRev

/**
 * Source code plus exercises from chapter 10 - Monoids. Monoid - category with one object
 */
object SourcePlusExercises {
  trait Monoid[A] {
    def op(a1: A, a2: A): A

    def zero: A
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
    override def op(a1: Int, a2: Int): Int = ???

    override def zero: Int = ???
  }
}
