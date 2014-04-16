package scala.chap10

import scala.chap10.ChapterExercises.Monoid

/**
 * Some extra things that I am learning
 */
object ChapterExtra {
  def listMonoidLift[A, B](f: A => B, bMon: Monoid[B]): List[A] => B = {
    def fun: List[A] => B = {
      case Nil => bMon.zero
      case a :: as => bMon.op(f(a), fun(as))
    }

    fun
  }
}
