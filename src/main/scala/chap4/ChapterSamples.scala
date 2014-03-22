package scala.chap4

import java.util.regex._

/**
 * Some code + exercises from chapter 4
 */
object ChapterSamples {
  case class Employee(name: String, dept: String)

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  //Ex 2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap((m) => mean(xs map((x) => Math.pow(m-x, 2.0))))

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  def mkMatcher_1(pat: String): Option[String => Boolean] =
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)

  def doesMatch(pat: String, s: String): Option[Boolean] =
    for {
      p <- mkMatcher_1(pat)
    } yield p(s)
}
