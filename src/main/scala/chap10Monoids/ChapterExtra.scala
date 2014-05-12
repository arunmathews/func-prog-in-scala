package scala.chap10Monoids

import scala.chap10Monoids.ChapterExercises.Monoid

/**
 * Some extra things that I am learning
 */
object ChapterExtra {
  //List is a free monoid i.e, given proof that B is a monoid and there is a function from A to B then there is a unique way
  //to generate monoid homomorphism from List[A] => B, i.e f(a1::a2) = f(a1) op f(a2)
  def listMonoidLift[A, B](f: A => B, bMon: Monoid[B]): List[A] => B = {
    def fun: List[A] => B = {
      case Nil => bMon.zero
      case a :: as => bMon.op(f(a), fun(as))
    }

    fun
  }
}
