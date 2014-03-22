package scala.chap5

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
 * Code samples from chapter 5
 */
object ChapterSamples {
  sealed trait Stream[+A] {
    def uncons: Option[Cons[A]]
    def isEmpty: Boolean = uncons.isEmpty

    //Ex 1
    def toListNonTailRecursive: List[A] = uncons match {
      case Some(x) => x.head :: x.tail.toListNonTailRecursive
      case None => Nil
    }

    def toListTailRecursive: List[A] = {
      @tailrec
      def accum(acc: List[A], stream: Stream[A]): List[A] = {
        stream.uncons match {
          case None => acc
          case Some(x) => accum(x.head :: acc, x.tail)
        }
      }

      accum(Nil, this).reverse
    }

    def toList: List[A] = {
      val buffer = new ListBuffer[A]

      @tailrec
      def accum(stream: Stream[A]): List[A] = {
        stream.uncons match {
          case None => buffer.toList
          case Some(x) => {
            buffer += x.head
            accum(x.tail)
          }
        }
      }

      accum(this)
    }

    def take(n: Int): Stream[A] = {
      if(n==0) Stream()
      else {
        uncons match {
          case Some(x) if n == 1 => Stream.cons(x.head, Stream())
          case Some(x) => Stream.cons(x.head, x.tail.take(n-1))
          case _ => Stream.empty
        }
      }
    }

    //Ex 3
    def takeWhile(p: A => Boolean): Stream[A] = {
      uncons match {
        case Some(x) if p(x.head) => Stream.cons(x.head, x.tail.takeWhile(p))
        case _ => Stream.empty
      }
    }

    def exists(p: A => Boolean): Boolean = {
      uncons match {
        case Some(x) => p(x.head) || x.tail.exists(p)
        case _ => false
      }
    }

    //Ex 6: Implement foldRight in child classes
    def foldRight[B](z: => B)(f: (A, => B) => B): B // =
      /*uncons match {
        case Some(c) => f(c.head, c.tail.foldRight(z)(f))
        case None => z
      }*/

    def existsFoldRight(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

    //Ex 4
    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    def forEach(p: A => Unit): Unit = foldRight(None)((a, b) => {p(a);b})

    //Ex 5
    def takeWhileFoldRight(p: A => Boolean): Stream[A] = {
      val x: Stream[A] = Stream.empty
      foldRight(x)((a, b) => if(p(a)) Stream.cons(a, b) else Stream.empty)
    }

    //Ex 6
    def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))

    def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h, t) => if(f(h)) Stream.cons(h, t) else t)

    def append[B >: A](s: Stream[B]): Stream[B] = foldRight(s)((h, t) => Stream.cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((h, t) => f(h) append t)

    def find(p: A => Boolean): Option[A] = filter(p).uncons.map(_.head)

    //Ex 13
    def mapUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
      _.uncons map {
        consA => (f(consA.head), consA.tail)
      }
    }

    def takeUnfold(n: Int): Stream[A] = Stream.unfold((this, n)) {
      case (s, nowN) if nowN > 0 => s.uncons map(nowS => (nowS.head, (nowS.tail, nowN-1)))
      case _ => None
    }

    def takeWhileUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
      s => s.uncons match {
        case Some(c) if p(c.head) => Some(c.head, c.tail)
        case _ => None
      }
    }

    def zip[B](that: Stream[B]): Stream[(A, B)] = Stream.unfold((this, that)) {
      case (s1, s2) =>
        (s1.uncons, s2.uncons) match {
          case (Some(c1), Some(c2)) => Some((c1.head, c2.head), (c1.tail, c2.tail))
          case _ => None
        }
    }

    def zipAllEndsAtFirstNone[B](that: Stream[B]): Stream[(Option[A],Option[B])] = Stream.unfold((this, that)) {
      case (s1, s2) =>
        (s1.uncons, s2.uncons) match {
          case (Some(c1), Some(c2)) => Some((Some(c1.head), Some(c2.head)), (c1.tail, c2.tail))
          case (Some(c1), None) => Some((Some(c1.head), None), (c1.tail, Stream.empty))
          case (None, Some(c2)) => Some((None, Some(c2.head)), (Stream.empty, c2.tail))
          case (None, None) => None
        }
    }

    def zipAll[B](that: Stream[B]): Stream[(Option[A],Option[B])] =
      zipWithAll(that)((_,_))

    def zipWithAll[B,C](that: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] = {
      val a = this map (Some(_)) append (Stream.constant(None))
      val b = that map (Some(_)) append (Stream.constant(None))
      Stream.unfold((a, b)) {
        case (s1,s2) => for {
          c1 <- s1.uncons
          c2 <- s2.uncons
        } yield (f(c1.head, c2.head), (c1.tail, c2.tail))
      }
    }

    //Ex 15
    def tails: Stream[Stream[A]] = Stream.unfold(this) {
      s => s.uncons match {
        case Some(c) => Some(s, c.tail)
        case _ => None
      }
    } append Stream.empty

    //Ex 16
    def scanRight[B](z: B)(f: (A,=>B) => B): Stream[B] = foldRight((z, Stream(z)))((elem, tuple) => {
      val nextB = f(elem, tuple._1)

      (nextB, Stream.cons(nextB, tuple._2))
    })._2
  }

  object Empty extends Stream[Nothing] {
    val uncons = None

    //Ex 6
    def foldRight[B](z: => B)(f: (Nothing, => B) => B): B = z
  }

  sealed abstract class Cons[+A] extends Stream[A] {
    def head: A
    def tail: Stream[A]
    val uncons = Some(this)

    //Ex 6
    def foldRight[B](z: => B)(f: (A, => B) => B): B = f(head, tail.foldRight(z)(f))
  }

  object Stream {
    def empty[A]: Stream[A] = Empty

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Cons[A] {
      lazy val tail: Stream[A] = tl

      lazy val head: A = hd
    }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

    //Ex 11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case None => Stream.empty
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }

    //Ex 8
    def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

    //Ex 9
    def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))
  }

  //Ex 10
  val fibs: Stream[Int] = {
    def sumPrev(fst: Int, snd: Int): Stream[Int] = Stream.cons(fst, sumPrev(snd, fst + snd))

    sumPrev(0, 1)
  }

  //Ex 12
  val fibsUnfold: Stream[Int] = Stream.unfold((0, 1))(tuple => Option(tuple._1, (tuple._2, tuple._1 + tuple._2)))

  def fromUnfold(n: Int): Stream[Int] = Stream.unfold(n)(from => Option(from, from+1))

  def constantUnfold[A](a: A): Stream[A] = Stream.unfold(a)(newA => Option(a, a))

  val onesUnfold: Stream[Int] = constantUnfold(1)

  //Ex 14
  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = s.zipAll(s2).takeWhile(!_._2.isEmpty) forAll {
    case (Some(h1), Some(h2)) if h1 == h2 => true
    case _ => false
  }

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean = s1.tails exists(startsWith(_, s2))
}