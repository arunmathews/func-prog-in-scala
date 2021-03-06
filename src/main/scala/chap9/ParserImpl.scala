package scala.chap9

import scala.chap9.ChapterExercisesSource.{Location, ParseError, Parsers}
import scala.util.matching.Regex
import ParserImplTypes._

/**
 *  An Implementation of the Parser trait
 */
object ParserImplTypes {
  type Parser[+A] = Location => Result[A]

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, isCommitted = false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, false) if isCommitted => Failure(e, isCommitted = true)
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  def firstNonMatchingIndex(s: String, s2: String, offset: Int): Int = {
    val lengthToCheck = (s.length - offset) min s2.length

    val maybeNoMatch = (0 to lengthToCheck-1).toStream.flatMap(i =>
      if (s.charAt(i+offset) != s2.charAt(i)) {
        Some(i)
      }
    else {
        None
      }
    ).headOption

    maybeNoMatch.getOrElse {
      if ((s.length - offset) >= s2.length) -1
      else s.length - offset
    }
  }
}

object ParserImpl extends Parsers[Parser]{

  override def string(w: String): Parser[String] = {
    val msg = "'" + w + "'"
    s => {
      val i = firstNonMatchingIndex(s.input, w, s.offset)
      if (i == -1) {
        Success(w, w.length)
      }
      else {
        Failure(s.advanceBy(i).toError(msg), i != 0)
      }
    }
  }

  //If parsing fails ParseError should have the 'msg'
  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  //'msg' will be added to error stack if parser wrapped with scope is run and it fails
  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(s, msg))

  //Delays committing during parsing. If p fails midway through parsing undo the commit
  override def attempt[A](p: Parser[A]): Parser[A] =
    s => p(s).uncommit

  //2nd argument is lazy. Try the 2nd parser only if the first does not succeed
  override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] =
    s => p1(s) match {
      case f@Failure(e, committed) if !committed => p2(s)
      case r => r
    }

  override def regex(r: Regex): Parser[String] = {
    val msg = "regex: " + r
    s => {
      val splitS = s.input.substring(s.offset)
      r.findPrefixOf(splitS) match {
        case None => Failure(s.toError(msg), isCommitted = false)
        case Some(m) => Success(m, m.length)
      }
    }
  }

  override def succeed[A](a: A): Parser[A] =
    s => Success(a, 0)

  override def flatMap[A, B](pa: Parser[A])(f: A => Parser[B]): Parser[B] =
    s => pa(s) match {
      case Success(a, n) =>
        f(a)(s.advanceBy(n)).addCommit(n != 0) match {
          case Success(b, n1) => Success(b, n + n1)
          case e1@Failure(_, _) => e1
        }
      case e@Failure(_, _) => e
    }

  //Return string up to where parsing with p is successful
  override def slice[A](p: Parser[A]): Parser[String] = s => p(s) match {
    case Success(_, n) => Success(s.input.substring(s.offset, s.offset + n), n)
    case f@Failure(_, _) => f
  }

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = p(Location(input)) match {
    case Success(a, _) => Right(a)
    case Failure(error, _) => Left(error)
  }
}
