package scala.chap9

import scala.chap9.ChapterExercisesSource.{Location, ParseError, Parsers}
import scala.util.matching.Regex
import ParserImplTypes._

/**
 *  An Implementation of the Parser trait
 */
object ParserImplTypes {
  type Parser[+A] = Location => Result[A]

  trait Result[+A]

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]
}

object ParserImpl extends Parsers[Parser]{

  override def string(s: String): Parser[String] =
    (inputString: Location) =>
      if (inputString.input.startsWith(s)) {
        Success(s, s.length)
      }
      else {
        Failure(Location(inputString.input, inputString.offset).toError("Expected: " + s))
      }

  override def errorMessage(e: ParseError): String = ???

  override def errorLocation(e: ParseError): Location = ???

  //Ex 5. delayed evaluation of argument p
  override def wrap[A](p: => Parser[A]): Parser[A] = ???

  //If parsing fails ParseError should have the 'msg'
  override def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

  //'msg' will be added to error stack if parser wrapped with scope is run and it fails
  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

  //Delays committing during parsing. If p fails midway through parsing undo the commit
  override def attempt[A](p: Parser[A]): Parser[A] = ???

  //2nd argument is lazy. Try the 2nd parser only if the first does not succeed
  override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = ???

  override def regex(r: Regex): Parser[String] = {
    val msg = "regex" + r
    s => r.findPrefixOf(s.input) match {
      case None => Failure(s.toError(msg))
      case Some(m) => Success(m, m.length)
    }
  }


  override def flatMap[A, B](pa: Parser[A])(f: (A) => Parser[B]): Parser[B] = ???

  //Return string up to where parsing with p is successful
  override def slice[A](p: Parser[A]): Parser[String] = s => p(s) match {
    case Success(_, n) => Success(s.input.substring(s.offset, s.offset + n), n)
    case f@Failure(_) => f
  }

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???
}
