package scala.chap10Monoids

import scala.chap10Monoids.ChapterExercises._
import scala.language.higherKinds
import scala.chap3.Chapter.{Branch, Leaf, Tree}

/**
 * Foldable data structures
 */
trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(b: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(b: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  //Ex 16 my way
  def toListFM[A](fa: F[A]): List[A] = foldMap(fa)(a => List(a))(listMonoid)
  def toList[A](fa: F[A]): List[A] = foldRight(fa)(List[A]())(_ :: _)
}

//Ex 13
object ListFoldable extends Foldable[List] {
  override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))

  override def foldLeft[A, B](as: List[A])(b: B)(f: (B, A) => B): B = as.foldLeft(b)(f)

  override def foldRight[A, B](as: List[A])(b: B)(f: (A, B) => B): B = as.foldRight(b)(f)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]): B = foldMapV(as, mb)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(b: B)(f: (B, A) => B): B = as.foldLeft(b)(f)

  override def foldRight[A, B](as: IndexedSeq[A])(b: B)(f: (A, B) => B): B = as.foldRight(b)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldMap[A, B](as: Stream[A])(f: (A) => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))

  override def foldLeft[A, B](as: Stream[A])(b: B)(f: (B, A) => B): B = as.foldLeft(b)(f)

  override def foldRight[A, B](as: Stream[A])(b: B)(f: (A, B) => B): B = as.foldRight(b)(f)
}

//Ex 14
object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(b: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(b, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(b)(f))(f)
  }

  override def foldRight[A, B](as: Tree[A])(b: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, b)
    case Branch(l, r) => foldRight(l)(foldRight(r)(b)(f))(f)
  }
}

//Ex 15
object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = as.fold(mb.zero)(f)

  override def foldLeft[A, B](as: Option[A])(b: B)(f: (B, A) => B): B = as.foldLeft(b)(f)

  override def foldRight[A, B](as: Option[A])(b: B)(f: (A, B) => B): B = as.foldRight(b)(f)
}

