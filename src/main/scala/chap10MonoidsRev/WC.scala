package scala.chap10MonoidsRev

import scala.chap10MonoidsRev.SourcePlusExercises._
/**
 * WC class
 */
sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {
  //Ex 10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
      case (Stub(c1), Part(l, words, r)) => Part(c1 + l, words, r)
      case (Part(l, words, r), Stub(c2)) => Part(l, words, r + c2)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if((r1 + l2).isEmpty) 0 else 1), r2)
    }

    override def zero: WC = Stub("")
  }

  def unstub(s: String): Int = s.length min 1

  //Ex 11
  def wordCount(s: String): Int = {
    def getWC(as: String): WC = as.size match {
      case 0 => wcMonoid.zero
      case 1 => if(as == " ") Part("", 1, "") else Stub(as)
      case _ =>
        val (l, r) = as.splitAt(as.size/2)
        wcMonoid.op(getWC(l), getWC(r))
    }

    getWC(s) match {
      case Stub(ss) => unstub(ss)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  //Ex 11 foldMapV
  def wordCountFMV(s: String): Int = {
    def charToWC(c: Char) = if(c.isWhitespace) Part("", 0, "") else Stub(c.toString)

    foldMapV(s.toIndexedSeq, wcMonoid)(charToWC) match {
      case Stub(ss) => unstub(ss)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }
}
