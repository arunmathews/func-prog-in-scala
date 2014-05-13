package scala.chap12Applicative

import scala.chap11Monads.SourceExercises.Functor

/**
 * Source code and exercises from chapter 12
 */
object SourceExercises {
  trait Applicative[F[_]] extends Functor[F] {
    //primitive combinators
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

    def unit[A](a: => A): F[A]

    //derived combinators
    override def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))

    def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = la.foldRight(unit(List[B]()))((a, lfb) => map2(f(a), lfb)(_ :: _))

    //Ex 1
    def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(fa => fa)

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

    def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((a, b) => (a, b))
  }
}
