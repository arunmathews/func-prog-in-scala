package scala.chap2

import scala.annotation.tailrec

/**
 * Exercises for chapter 2
 */
object Exercises {
  //Ex 1
  def fib(n: Int):Int = {

    @tailrec
    def accumulate(num:Int, acc1:Int, acc2:Int):Int = {
      if(num==n) acc1
      else accumulate(num+1, acc1+acc2, acc1)
    }

    n match {
      case 1 => 0
      case 2 => 1
      case neg if neg <= 0 => throw new RuntimeException("No fibonacci for negative numbers")
      case _ => accumulate(3,1,1)
    }
  }

  //Ex 2
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {

    @tailrec
    def loop(n: Int): Boolean = {
      if(n==as.length-1) true
      else if(gt(as(n), as(n+1))) loop(n+1)
      else false
    }

    loop(0)
  }

  //Ex 3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b:B) => f(a,b)

  //Ex 4
  def uncurry[A, B, C](f: A => B => C):(A, B) => C = (a: A, b: B) => f(a)(b)

  //Ex 5
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
