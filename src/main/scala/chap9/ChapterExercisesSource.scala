package scala.chap9

import scala.language.implicitConversions
/**
 * Chapter and exercises code for chapter 9
 */
object ChapterExercisesSource {
  trait Parser[+A]
  trait ParseError

  trait Parsers[ParseError, Parser[+_]] {
    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    //should satisfy run(char(c))(c.toString) == Right(c)
    implicit def char(c: Char): Parser[Char]

    //should satisfy run(string(s))(s) == Right(s)
    implicit def string(s: String): Parser[String]

    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

    case class ParserOps[A](p: Parser[A]) {
      def |[B >:A](p2: Parser[B]): Parser[B] = or(p, p2)
    }
  }
}
