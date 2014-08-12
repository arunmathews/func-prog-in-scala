package scala.chap11MonadsRev

import scala.chap6.StatePattern.State
import scala.chap7.SampleExercises._
import scala.chap8.ChapterSamples.Gen
import scala.chap9.ChapterExercisesSource.Parsers
import scala.language.higherKinds

/**
 * Revision of chap 11 Monads after a break
 */
object SourcePlusExercises {
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

    //Ex 3 Got this right. Yay
    def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldRight(unit(List[A]()))((ma, mSeq) => map2(ma, mSeq)(_ ::_))

    def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = la.foldRight(unit(List[B]()))((a, mSeq) => map2(f(a), mSeq)(_ :: _))
    //End Ex 3

    //Ex 4
    def replicateMMatch[A](n: Int, ma: F[A]): F[List[A]] = n match {
      case 0 => unit(List[A]())
      case _ => map2(ma, replicateM(n-1, ma))(_ :: _)
    }

    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))
    //End Ex 4

    //Ex 5
    //ListMonad replicateM -  Create a list of lists where each list is of length n. The elements will be selected from
    // the input list. So if there are x elements in the input list then the number of lists generated = x ^ n

    //OptionMonad replicatwM - Create an Option based on the input option. None gives None. Some(x) will give
    //Some(List(x repeated n times)

    //Generally replicateM will create a list of monadic values and then combine them into a single list value where
    //the combination function depends on the monad. For option None will short circuit the whole operation to None

    def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

    //Ex 6
    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
      ms match {
        case Nil => unit(Nil)
        case x::xs => flatMap(f(x))(b => if(!b) filterM(xs)(f) else map(filterM(xs)(f))(x :: _))
      }
    }
    //Ex 6 done
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
