package scala.chap9

import scala.language.implicitConversions
/**
 * Chapter and exercises code for chapter 9
 */
object ChapterExercisesSource {
  trait Parser[+A]
  trait ParseError

  trait Parsers[ParseError, Parser[+_]] { self =>
    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    //should satisfy run(char(c))(c.toString) == Right(c)
    implicit def char(c: Char): Parser[Char]

    //should satisfy run(string(s))(s) == Right(s)
    implicit def string(s: String): Parser[String]

    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

    def map[A, B](a : Parser[A])(f: A => B): Parser[B]

    def map2[A, B, C](a: Parser[A], b: Parser[B])(f: (A, B) => C): Parser[C]

    def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

    def many[A](p: Parser[A]): Parser[List[A]]

    def zeroOrMore(c: Char): Parser[Int] = map(many(char(c)))(l => l.size)

    def oneOrMore(c: Char): Parser[Int] = map(map2(char(c), many(char(c)))(_ :: _))(l => l.size)

    def zeroAOneB(c1: Char, c2: Char): Parser[(Int, Int)] = map2(zeroOrMore(c1), oneOrMore(c2))((_, _))

    def zeroCount(c: Char): Parser[Int] =

    case class ParserOps[A](p: Parser[A]) {
      def |[B >:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def or[B >:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    }
  }
}
