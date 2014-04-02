package scala.chap9

import scala.language.implicitConversions
import scala.chap8.ChapterSamples.{Prop, Gen}
import scala.language.higherKinds

/**
 * Chapter and exercises code for chapter 9
 */
object ChapterExercisesSource {
  trait Parser[+A]
  trait ParseError

  trait Parsers[ParseError, Parser[+_]] { self =>
    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    //should satisfy run(char(c))(c.toString) == Right(c)
    implicit def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

    def succeed[A](a: A): Parser[A] =
      string("").map(_ => a)

    //should satisfy run(string(s))(s) == Right(s)
    implicit def string(s: String): Parser[String]

    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    def slice[A](p: Parser[A]): Parser[String]

    def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

    def many[A](p: Parser[A]): Parser[List[A]]

    def zeroOrMoreList(c: Char): Parser[Int] = char(c).many.map(_.size)

    //Why is this better than the method above ? Size operation on string is constant time, on list is linear time
    def zeroOrMore(c: Char): Parser[Int] = char(c).many.slice.map(_.size)

    //Ex 1
    def oneOrMore(c: Char): Parser[Int] = char(c).map2(char(c).many.slice)(_ +: _).map(_.size)

    def zeroAOneB(c1: Char, c2: Char): Parser[(Int, Int)] = zeroOrMore(c1).map2(oneOrMore(c2))((_, _))

    def map[A, B](pa: Parser[A])(f: A => B): Parser[B]

    def flatMap[A, B](pa: Parser[A])(f: A => Parser[B]): Parser[B]

    //Ex 1
    def map2[A, B, C](pa: Parser[A], pb: Parser[B])(f: (A, B) => C): Parser[C] =
      map(product(pa, pb))(f.tupled)

    def product[A, B](pa: Parser[A], pb: Parser[B]): Parser[(A, B)]
    //def zeroCount(c: Char): Parser[Int] =

    case class ParserOps[A](p: Parser[A]) {
      def |[B >:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def or[B >:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def map2[B, C](pb: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, pb)(f)
      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
      def **[B](p2: Parser[B]) = self.product(p, p2)
      def product[B](p2: Parser[B]) = self.product(p, p2)
      def slice: Parser[String] = self.slice(p)
      def many: Parser[List[A]] = self.many(p)
    }

    object Laws {
      def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
        Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

      def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
        equal(p, p.map(a => a))(in)

      def unitLaw[A](a: A)(in: Gen[String]): Prop =
        Prop.forAll(in)(s => run(succeed(a))(s) == Right(a))
    }
  }
}
