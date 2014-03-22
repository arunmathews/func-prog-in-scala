/**
 * Tests for exercises in chapter 3
 */

import scala.chap3.Chapter._
import scala.chap3.Chapter.List._

//Ex 1
val x = List(1,2,3,4,5) match {
  case Cons(y, Cons(2, Cons(4, _))) => y
  case Nil => 42
  case Cons(z, Cons(y, Cons(3, Cons(4, _)))) => z + y
  case Cons(h, t) => h + sum(t)
  case _ => 101
}
val xs = List(1,2,3,4,8)

val ex1 = dropWhile(xs)((x) => x < 4)

//Ex 8
foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

length(List(1,2,3))

foldLeft(List(1,2,3), Nil:List[Int])((l:List[Int], a:Int) => Cons(a, l))

lengthLeft(List(1,2,3))

sumLeft(List(1, 2, 3))

reverse(List(1, 2, 3))

append(List(1,2,3), List(7,6,5))



add1(List(2,3,4))

map(List(1, 2, 3))((a:Int) => a * 2)

filter(List(1,2,3,4,5))((a: Int) => a%2==0)

flatMap(List(1,2,3))(i => List(i,i))


filter2(List(1,2,3,4,5))((a: Int) => a%2==0)

addCorresponding(List(1,2,3), List(4,5,6))

val list = List(1, 2, 3, 4)

hasSubsequence(list, List(1, 2))

hasSubsequence(list, List(2, 3))

hasSubsequence(list, List(4))

hasSubsequence(list, List(2, 4))
