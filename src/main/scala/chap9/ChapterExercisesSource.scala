package scala.chap9

import scala.language.implicitConversions
import scala.chap8.ChapterSamples.{Prop, Gen}
import scala.language.higherKinds
import scala.util.matching.Regex

/**
 * Chapter and exercises code for chapter 9
 */
object ChapterExercisesSource {
  trait Parser[+A]
  trait ParseError

  trait Parsers[ParseError, Parser[+_]] { self =>
    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    //Primitives
    //should satisfy run(string(s))(s) == Right(s)
    implicit def string(s: String): Parser[String]

    //Return string up to where parsing with p is successful
    def slice[A](p: Parser[A]): Parser[String]

    def succeed[A](a: A): Parser[A] =
      string("").map(_ => a)

    def flatMap[A, B](pa: Parser[A])(f: A => Parser[B]): Parser[B]

    implicit def regex(r: Regex): Parser[String]

    //2nd argument is lazy. Try the 2nd parser only if the first does not succeed
    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

    def attempt[A](p: Parser[A]): Parser[A]

    def scope[A](msg: String)(p: Parser[A]): Parser[A]

    //Non primitives - defined in terms of primitives and other non primitives
    //Parse and then apply f
    //Ex 8
    def map[A, B](pa: Parser[A])(f: A => B): Parser[B] =
      flatMap(pa)(a => succeed(f(a)))

    //Ex 1, Ex 7
    //Why is 2nd argument lazy ? Use the 2nd parser only if the 1st parser does not error
    def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
      flatMap(pa)(a => map(pb)(b => f(a, b)))

    //Ex 7
    //2nd argument is lazy. Try the 2nd parser only if the first parser succeeds
    def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] =
      flatMap(pa)(a => map(pb)(b => (a, b)))

    //should satisfy run(char(c))(c.toString) == Right(c)
    implicit def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    //Ex 3
    //For this to terminate map2 should evaluate 2nd argument lazily
    def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List[A]())

    //Ex 4
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
      case c if c == 0 => succeed(List[A]())
      case _ => map2(p, listOfN(n-1, p))(_ :: _)
    }

    //Ex 5. delayed evaluation of argument p
    def wrap[A](p: => Parser[A]): Parser[A]

    //Ex6
    def manyWrap[A](p: Parser[A]): Parser[List[A]] = map2(p, wrap(many(p)))(_ :: _) or succeed(List[A]())

    def zeroOrMoreList(c: Char): Parser[Int] = char(c).many.map(_.size)

    //Why is this better than the method above ? Bcoz size operation on string is constant time, on list is linear time
    def zeroOrMore(c: Char): Parser[Int] = char(c).many.slice.map(_.size)

    //Ex 1
    def oneOrMore(c: Char): Parser[Int] = char(c).map2(char(c).many.slice)(_ +: _).map(_.size)

    def zeroC1AndOneC2(c1: Char, c2: Char): Parser[(Int, Int)] = zeroOrMore(c1).map2(oneOrMore(c2))((_, _))

    def zeroC1OneC2(c1: Char, c2: Char): Parser[(Int, Int)] = zeroOrMore(c1) ** oneOrMore(c2)

    def skipL[B](p: Parser[Any], pb: => Parser[B]): Parser[B] =
      map2(slice(p), pb)((_, b) => b)

    def skipR[A](pa: Parser[A], p: => Parser[Any]): Parser[A] =
      map2(pa, slice(p))((a, _) => a)

    def opt[A](p: Parser[A]): Parser[Option[A]] =
      p.map(Some(_)) or succeed(None)

    def token[A](p: Parser[A]): Parser[A] =
      attempt(p) <* whiteSpace

    //Implicit conversion to regex parser
    //Zero or more whitespace characters
    def whiteSpace: Parser[String] = "\\s*".r

    //Implicit conversion to regex parser
    //One or more digits
    def digits: Parser[String] = "\\d+".r

    def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
      start *> p <* stop

    def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = sep1(p, p2) or succeed(List[A]())

    def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
      map2(p, many(p2 *> p))(_ :: _)
    
    //Ex 6
    def digitAndNumberChars(c: Char): Parser[List[Char]] =
      digits.flatMap(s => listOfN(s.toInt, char(c)))

    //Ex 6 from solutions. This is better because we are getting the count of the repeated character
    def countChar(c: Char): Parser[Int] = for {
        intString <- "[0-9]+".r //Implicit conversion to regex parser
        n = intString.toInt
        _ <- listOfN(n, c)
      } yield n

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
      def *>[B](pb: => Parser[B]) = self.skipL(p, pb)
      def <*(pb: => Parser[Any]) = self.skipR(p, pb)
      def sep(pb: Parser[Any]) = self.sep(p, pb)
      def sep1(pb: Parser[Any]) = self.sep1(p, pb)
      def scope(msg: String): Parser[A] = self.scope(msg)(p)
    }

    object Laws {
      def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
        Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

      def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
        equal(p, p.map(a => a))(in)

      def unitLaw[A](a: A)(in: Gen[String]): Prop =
        Prop.forAll(in)(s => run(succeed(a))(s) == Right(a))

      //Ex 2. Product associative
      def productLaw[A, B, C](pa: Parser[A], pb: Parser[B], pc: Parser[C])(in: Gen[String]): Prop =
        equal(((pa ** pb) ** pc).map({case ((a, b), c) => (a, b, c)}), (pa ** (pb ** pc)).map({case (a, (b, c)) => (a, b, c)}))(in)

      //Map of product = product of map.
      def productMapLaw[A, B, C, D](pa: Parser[A], pb: Parser[B])(f: A => C)(g: B => D)(in: Gen[String]): Prop =
        equal((pa ** pb).map({case (a, b) => (f(a), g(b))}), pa.map(f) ** pb.map(g))(in)
    }
  }
}
