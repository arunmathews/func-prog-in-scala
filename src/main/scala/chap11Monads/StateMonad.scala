package scala.chap11Monads

import scala.chap6.StatePattern.State
import scala.chap11Monads.SourceExercises.Monad

/**
 * Some classes, objects related to the state monad
 */
//Ex 2
class StateMonad[S] {
  type StateS[A] = State[S, A]

  val monad = new Monad[StateS] {
    override def unit[A](a: => A): StateS[A] = State.unit(a)

    override def flatMap[A, B](fa: StateS[A])(f: (A) => StateS[B]): StateS[B] = fa.flatMap(f)
  }
}

object StateMonad {
  //Nice way - Anonymous class inline
  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State.unit(a)

    override def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] = fa flatMap f

    def getState: State[S, S] = State(s => (s, s))

    def setState(s: => S): State[S, Unit] = State(_ => ((), s))
  }

  val F = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
      n <- State.get
      xs <- acc
      _ <- State.set(n+1)
    } yield (n, a) :: xs).run(0)._1.reverse

  def zipTest[A](as: List[A]) =
    as.foldLeft(State.unit(List[(Int, A)]()))((acc, a) => for {
      n <- State.get
      xs <- acc
      _ <- State.set(n+1)
    } yield (n, a) :: xs)


}
