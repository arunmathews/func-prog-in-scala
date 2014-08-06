package scala.chap11MonadsRev

import scala.chap6.StatePattern.State
import scala.chap7.SampleExercises._
import scala.chap8.ChapterSamples.Gen
import scala.chap9.ChapterExercisesSource.{Parser, Parsers}

/**
 * Revision of chap 11 Monads after a break
 */
class SourcePlusExercises {
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

    def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
  }

  val listFunctor = new Functor[List] {
    override def map[A, B](la: List[A])(f: (A) => B): List[B] = la map f
  }

  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      flatMap(fa)(a => map(fb)(b => f(a, b)))

    //Ex 3
    def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldRight(unit(List[A]()))((ma, mSeq) => map2(ma, mSeq)(_ ::_))

    //Got this right. Yay
    def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = la.foldRight(unit(List[B]()))((a, mSeq) => map2(f(a), mSeq)(_ :: _))
    //End Ex 3
  }

  object Monad {
    val genMonad = new Monad[Gen] {
      override def unit[A](a: => A): Gen[A] = Gen.unit(a)

      override def flatMap[A, B](fa: Gen[A])(f: (A) => Gen[B]): Gen[B] = fa flatMap f
    }

    //Ex 1
    val parMonad = new Monad[Par] {
      override def unit[A](a: => A): Par[A] = Par.unit(a)

      override def flatMap[A, B](fa: Par[A])(f: (A) => Par[B]): Par[B] = fa flatMap f
    }

    def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
      override def unit[A](a: => A): P[A] = p.succeed(a)

      override def flatMap[A, B](fa: P[A])(f: (A) => P[B]): P[B] = p.flatMap(fa)(f)
    }

    val optionMonad = new Monad[Option] {
      override def unit[A](a: => A): Option[A] = Option(a)

      override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa flatMap f
    }

    val streamMonad = new Monad[Stream] {
      override def unit[A](a: => A): Stream[A] = Stream(a)

      override def flatMap[A, B](fa: Stream[A])(f: (A) => Stream[B]): Stream[B] = fa flatMap f
    }

    val listMonad = new Monad[List] {
      override def unit[A](a: => A): List[A] = List(a)

      override def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] = fa flatMap f
    }

    //End Ex 1

    //Ex 2
    class StateMonadsS[S] {
      type StateS[A] = State[S, A]

      val monad = new Monad[StateS] {
        override def unit[A](a: => A): StateS[A] = State.unit(a)

        override def flatMap[A, B](fa: StateS[A])(f: (A) => StateS[B]): StateS[B] = fa flatMap f
      }
    }

    //Nice. Create anonymous class and project out its type member f
    def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
      override def unit[A](a: => A): State[S, A] = State.unit(a)

      override def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] = fa flatMap f
    }
    //End Ex 2
  }
}
