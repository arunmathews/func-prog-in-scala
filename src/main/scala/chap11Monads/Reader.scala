package scala.chap11Monads

import scala.chap11Monads.SourceExercises.Monad

/**
 * Reader Monad
 */
//Ex 21
case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f]{

    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(r => {
      val a = st.run(r)

      f(a).run(r)
    })
  }

  def ask[R]: Reader[R, R] = Reader(r => r)
}
