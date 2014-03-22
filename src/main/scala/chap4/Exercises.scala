package scala.chap4

import scala.chap4.ChapterSamples._

/**
 *
 */
object Exercises {
  //Ex 3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      x <- a
      y <- b
    } yield f(x,y)

  def map3[A,B,C](a: Option[A], b:Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(x => b map(y => f(x, y)))

  //Ex 4
  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = map2(mkMatcher(pat1), mkMatcher(pat2))((f1, f2) => f1(s) && f2(s))

  //Ex 5
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case x::xs => x flatMap(xx => sequence(xs) map(xx :: _))
    }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

  //Ex 6
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case x::xs => f(x) flatMap(xx => traverse(xs)(f) map(xx :: _))
    }

  def  traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case x::xs => map2(f(x), traverse2(xs)(f))(_ :: _)
    }

  def sequenceTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
}
