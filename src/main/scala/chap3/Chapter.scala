package scala.chap3

import scala.annotation.tailrec

/**
 * Code from chapter 3
 */
object Chapter {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      l match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    //Ex 10
    @tailrec
    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
      l match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }

    //Ex 13
    //Nice
    def foldRightUsingLeftAndReverse[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((b, a) => f(a, b))

    //Very nice
    //Build up a chain of functions that get evaluated when finally called with the parameter z of type B
    def foldRightUsingLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(l, (b:B) => b)((g,a) => b => g(f(a, b)))(z)
    //Nice symmetry
    def foldLeftUsingRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, (b:B) => b)((a,g) => b => g(f(b, a)))(z)

    def sum2(l: List[Int]) =
      foldRight(l, 0)((x, y) => x + y)

    def product2(l: List[Double]) =
      foldRight(l, 1.0)(_ * _)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    //Ex 2
    def tail[A](list: List[A]): List[A] = {
      list match {
        case Nil => Nil
        case Cons(x, xs) => xs
      }
    }

    //Ex 3
    def setHead[A](newHead: A, list: List[A]): List[A] = {
      list match {
        case Nil => Nil
        case Cons(x, xs) => Cons(newHead, xs)
      }
    }

    //Ex 4
    def drop[A](l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(x, xs) => drop(xs, n - 1)
      }
    }

    //Ex 5
    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
      l match {
        case Nil => Nil
        case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else l
      }
    }

    //Ex 6
    def init[A](l: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(x, Nil) => Nil
        case Cons(x, Cons(y, ys)) => Cons(x, init(Cons(y, ys)))
      }
    }

    //Ex 7. Ans - Not possible

    //Ex 9
    def length[A](l: List[A]): Int = foldRight(l, 0)((a, b) => b + 1)

    //Ex 11
    def sumLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

    def productLeft(l: List[Double]): Double = foldLeft(l, 0.0)(_ * _)

    def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((b, a) => b + 1)

    //Ex 12
    def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((l: List[A], z: A) => Cons(z, l))

    //Ex 14
    def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((a, l) => Cons(a, l))

    //Ex 15
    def concatLists[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])((subL, concL) => append(subL, concL))

    //Ex 16
    def add1(lInt: List[Int]): List[Int] = foldRight(lInt, Nil: List[Int])((num, lMap) => Cons(num + 1, lMap))

    //Ex 17
    def DoubleToString(lDouble: List[Double]): List[String] = foldRight(lDouble, Nil: List[String])((num, lMap) => Cons(num.toString, lMap))

    //Ex 18
    def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a, lb) => Cons(f(a), lb))

    //Ex 19
    def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, Nil: List[A])((a, la) => if (f(a)) Cons(a, la) else la)

    //Ex 20
    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, Nil: List[B])((a, lb) => append(f(a), lb))

    //Ex 21
    def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)((a) => if(f(a)) List(a) else Nil)

    //Ex 22
    def addCorresponding(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
      case (Cons(x, xs), Cons(y, ys)) => Cons(x+y, addCorresponding(xs,ys))
      case _ => Nil
    }

    //Ex 23
    def combineCorresponding[A, B](l1: List[A], l2:List[A])(f: (A, A) => B): List[B] = (l1, l2) match
    {
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), combineCorresponding(xs,ys)(f))
      case _ => Nil
    }

    //Ex 24
    def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {

      def isEqual(subl: List[A], subSub: List[A]): Boolean = (subl, subSub) match {
        case (Cons(x, xs), Cons(y, ys)) => if(x==y) isEqual(xs, ys) else false
        case (_, Nil) => true
        case _ => false
      }

      sub match {
        case Nil => true
        case Cons(head, tail) => l match {
          case Cons(z, xz) => {
            if(if(z==head) isEqual(l, sub) else false) true
            else hasSubsequence(xz, sub)
          }
          case Nil => false
        }
      }
    }
  }

  //Tree exercises
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    //Ex 25
    def size[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + 1 + size(r)
    }

    //Ex 26
    def maximum(tree: Tree[Int]): Int = tree match {
      case Leaf(a) => a
      case Branch(l, r) => maximum(l) max maximum(r)
    }

    //Ex 27
    def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 0
      case Branch(l, r) => (depth(l) max depth(r)) + 1
    }

    //Ex 28
    def map[A, B](tree: Tree[A])( f: A => B): Tree[B] = tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    //Ex 29
    def fold[A, B](t: Tree[A])(f: A => B)(b: (B, B) => B): B = t match {
      case Leaf(a) => f(a)
      case Branch(l, r) => b(fold(l)(f)(b), fold(r)(f)(b))
    }

    def sizeFold[A](t: Tree[A]): Int = fold(t)((a:A) => 1)((b1:Int, b2: Int) => b1 + b2 + 1)

    def maximumFold(t: Tree[Int]): Int = fold(t)((a: Int) => a)((b1: Int, b2: Int) => b1 max b2)

    def depthFold[A](t: Tree[A]): Int = fold(t)((a: A) => 0)((dl: Int, dr: Int) => (dl max dr) + 1)

    def leaf[A](a: A): Tree[A] = Leaf(a)
    def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)

    def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)((a: A) => leaf(f(a)))((l: Tree[B], r: Tree[B]) => branch(l, r))
  }
}
