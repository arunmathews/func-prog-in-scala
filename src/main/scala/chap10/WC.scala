package scala.chap10

import scala.chap10.ChapterExercises._

/**
 * Doing word count in parallel
 */
sealed trait WC

case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {
  //Ex 11
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override val zero: WC = Stub("")

    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
      case (Stub(c1), Part(l1, w, l2)) => Part(c1 + l1, w, l2)
      case (Part(l1, w, l2), Stub(c2)) => Part(l1, w, l2 + c2)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
  }

  //Ex 12 - spaces are not words. other chars count towards words
  def wordCount(s: String): Int = {
    def findWC(as: String): WC = as.size match {
      case 0 => wcMonoid.zero
      case 1 => if(as==" ") Part("", 0, "") else Stub(as)
      case _ =>
        val (l, r) = as.splitAt(as.size/2)
        wcMonoid.op(findWC(l), findWC(r))
    }

    findWC(s) match {
      case Stub(p) if p.isEmpty => 0
      case Stub(p) => 1
      case Part(l, w, r) => w + (if (!l.isEmpty) 1 else 0) + (if (!r.isEmpty) 1 else 0)
    }
  }

  //Ex 12 - foldMapV will do the splitting for you
  def wordCountflv(s: String): Int = {
    def wc(c: Char): WC = if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)

    def unstub(s: String): Int = s.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(str) => unstub(str)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }
}