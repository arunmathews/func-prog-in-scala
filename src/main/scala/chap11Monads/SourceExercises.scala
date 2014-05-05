package scala.chap11Monads

import scala.language.higherKinds
import scala.chap8.ChapterSamples.Gen
import scala.chap7.SampleExercises._
import scala.chap9.ChapterExercisesSource._
import scala.chap6.StatePattern.State

/**
 * Source code from chapter and exercises for chapter 10 - Monads and Functors
 */
object SourceExercises {
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))
  }

  val listFunctor = new Functor[List] {
    override def map[A, B](as: List[A])(f: (A) => B): List[B] = as map f
  }

  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    override def map[A, B](ma: F[A])(f: A => B): F[B] =
      flatMap(ma)(a => unit(f(a)))

    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    //Ex 3
    def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldRight(unit(List[A]()))((fa, fla) => map2(fa, fla)(_ :: _))

    def traverseMine[A, B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la.map(f))

    def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = la.foldRight(unit(List[B]()))((a, flb) => map2(f(a), flb)(_ :: _))

    //Ex 4
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

    //Ex 5
    //List monad - list of lists
    //Option monad - All some then some of list. Otherwise None. Combination depending on the monad.

    def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

    //Ex 6
    def filterMMap2[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
      ms match {
        case Nil => unit(Nil)
        case x::xs =>
          map2(f(x), filterMMap2(xs)(f))((b, la) => if (b) x::la else la)
      }

    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match {
      case Nil => unit(Nil)
      case x::xs =>
        flatMap(f(x))(b => if (!b) filterM(xs)(f) else map(filterM(xs)(f))(x :: _))
    }

    //Ex 6 What does the above function mean - Monad will determine how the filtering function is run. For option this
    // can stop early on none. For Par filtering is done in parallel.

    //Ex 9
    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

    //Ex 10
    def flatMapComp[A, B](ma: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => ma, f)(())

    //Ex 13
    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(fa => fa)

    //Ex 14
    def flatMapJoin[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

    def composeJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))
  }

  object Monad {
    val genMonad = new Monad[Gen] {
      override def unit[A](a: => A): Gen[A] = Gen.unit(a)

      override def flatMap[A, B](ma: Gen[A])(f: (A) => Gen[B]): Gen[B] = ma flatMap f
    }

    //Ex 1
    val parMonad = new Monad[Par] {
      override def unit[A](a: => A): Par[A] = Par.unit(a)

      override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = ma flatMap f
    }

    def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
      override def unit[A](a: => A): P[A] = p.succeed(a)

      override def flatMap[A, B](ma: P[A])(f: (A) => P[B]): P[B] = p.flatMap(ma)(f)
    }

    val optionMonad = new Monad[Option] {
      override def unit[A](a: => A): Option[A] = Option(a)

      override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma flatMap f
    }

    val streamMonad = new Monad[Stream] {
      override def unit[A](a: => A): Stream[A] = Stream(a)

      override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma flatMap f
    }

    val listMonad = new Monad[List] {
      override def unit[A](a: => A): List[A] = List(a)

      override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma flatMap f
    }

    //Ex 2
    class StateMonads[S] {
      type StateS[A] = State[S, A]

      val monad = new Monad[StateS] {
        override def unit[A](a: => A): StateS[A] = State.unit(a)

        override def flatMap[A, B](fa: StateS[A])(f: (A) => StateS[B]): StateS[B] = fa.flatMap(f)
      }
    }

    //Nice way - Anonymous class inline
    def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
      override def unit[A](a: => A): State[S, A] = State.unit(a)

      override def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] = fa flatMap f
    }

    //Ex 18
    case class Id[A](value: A) {
      def map[B](f: A => B): Id[B] = Id(f(value))

      def flatMap[B](f: A => Id[B]): Id[B] = f(value)
    }

    val IdMonad = new Monad[Id] {
      override def unit[A](a: => A): Id[A] = Id(a)

      override def flatMap[A, B](ida: Id[A])(f: (A) => Id[B]): Id[B] = ida flatMap f
    }
  }
}
