package scala.chap4

/**
 * Code from chapter 4
 */
object Chapter {
  sealed trait Option[+A] {
    //Ex 1
    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(a) => a
      case None => default
    }

    //Nice
    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
    def orElse[B >: A](ob: => Option[B]): Option[B] = this map(a => Some(a)) getOrElse ob
    def filter(f: A => Boolean): Option[A] = flatMap(a => if(f(a)) Some(a) else None)
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  sealed trait Either[+E, +A] {
    //Ex 7
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(r) => Right(f(r))
      case Left(l) => Left(l)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(r) => f(r)
      case Left(l) => Left(l)
    }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(r) => Right(r)
      case Left(_) => b
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        x <- this
        y <- b
      } yield f(x, y)
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    a match {
      case Nil => Right(Nil)
      case x::xs => f(x).map2(traverse(xs)(f))(_ :: _)
    }

  def traverse_1[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    a.foldRight[Either[E, List[B]]](Right(Nil))((aa, bb) => f(aa).map2(bb)(_::_))

  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = traverse(a)(x => x)

  //Ex 9
  sealed trait Partial[+A, +B] {
      def map[E](f: B => E): Partial[A, E] = this match {
        case Success(b) => Success(f(b))
        case Errors(s) => Errors(s)
      }
  }

  case class Errors[+A](get: Seq[A]) extends Partial[A, Nothing]
  case class Success[+B](get: B) extends Partial[Nothing, B]


}
