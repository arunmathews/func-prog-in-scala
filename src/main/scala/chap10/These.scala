package scala.chap10

import scala.chap10.ChapterExercises.Monoid

/**
 * An alternative to Either that satisfies the monoid laws
 */
sealed trait These[+A, +B]
case class This[A](a: A) extends These[A, Nothing]
case class That[B](b: B) extends These[Nothing, B]
case class Both[A, B](a: A, b: B) extends These[A, B]

object These {
  def theseMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[These[A, B]] = new Monoid[These[A, B]] {
    override val zero: These[A, B] = Both(a.zero, b.zero)

    override def op(x: These[A, B], y: These[A, B]): These[A, B] = (x, y) match {
      case (This(a1), This(a2)) => This(a.op(a1, a2))
      case (That(b1), That(b2)) => That(b.op(b1, b2))
      case (This(a1), That(b1)) => Both(a1, b1)
      case (That(b1), This(a1)) => Both(a1, b1)
      case (Both(a1, b1), This(a2)) => Both(a.op(a1, a2), b1)
      case (Both(a1, b1), That(b2)) => Both(a1, b.op(b1, b2))
      case (This(a1), Both(a2, b2)) => Both(a.op(a1, a2), b2)
      case (That(b1), Both(a2, b2)) => Both(a2, b.op(b1, b2))
      case (Both(a1, b1), Both(a2, b2)) => Both(a.op(a1, a2), b.op(b1, b2))
    }
  }
}

