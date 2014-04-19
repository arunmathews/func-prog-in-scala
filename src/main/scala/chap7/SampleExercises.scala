package scala.chap7

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}
import scala.language.implicitConversions

/**
 * Samples and exercises from chap 7
 */
object SampleExercises {

  //Ex 1
  //sealed trait Par[+A] {

  //}

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def get(timeout: Long, unit: TimeUnit): A = get

    override def isDone: Boolean = true

    override def isCancelled: Boolean = false

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  private case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    var cache: Option[C] = None

    override def get(timeout: Long, unit: TimeUnit): C = compute(TimeUnit.MILLISECONDS.convert(timeout, unit))

    override def get(): C = compute(Long.MaxValue)

    override def isDone: Boolean = cache.isDefined

    override def isCancelled: Boolean = a.isCancelled || b.isCancelled

    override def cancel(interruptIfRunning: Boolean): Boolean = a.cancel(interruptIfRunning) || b.cancel(interruptIfRunning)

    private def compute(timeout: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.currentTimeMillis()
        val ar = a.get(timeout, TimeUnit.MILLISECONDS)
        val stop = System.currentTimeMillis()
        val at = stop - start
        val br = b.get(timeout - at, TimeUnit.MILLISECONDS)
        cache = Some(f(ar, br))

        cache.get
    }
  }

  type Par[A] = ExecutorService => Future[A]

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {

    def run(s: ExecutorService): Future[A] = p(s)

    def map2NoTimeout[B, C](b: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val af = p(es)
        val bf = b(es)

        UnitFuture(f(af.get, bf.get))
      }

    //Ex 3
    def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val af = p(es)
        val bf = b(es)

        Map2Future(af, bf, f)
      }

    def map[B](f: A => B): Par[B] = map2(Par.unit(()))((a, _) => f(a))

    def flatMap[B](f: A => Par[B]): Par[B] = (es: ExecutorService) => {
      val a = this.run(es).get()

      f(a).run(es)
    }

    def map3[B, C, D](pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] = {
      val abFunPar = this.map2(pb)((a, b) => f.curried(a)(b))

      abFunPar.map2(pc)((abFun, c) => abFun(c))
    }

    def map4[B, C, D, E](pb: Par[B], pc: Par[C], pd: Par[D])(f: (A, B, C, D) => E): Par[E] =
      this.map2(pb)((a, b) => f.curried(a)(b)).map2(pc)((abFun, c) => abFun(c)).map2(pd)((abcFun, d) => abcFun(d))

    def map5[B, C, D, E, F](pb: Par[B], pc: Par[C], pd: Par[D], pe: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
      this.map2(pb)((a, b) => f.curried(a)(b)).map2(pc)((abFun, c) => abFun(c)).map2(pd)((abcFun, d) => abcFun(d)).map2(pe)((abcdFun, e) => abcdFun(e))

  }

  object Par {

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    def fork[A](a: => Par[A]): Par[A] = {
      es => es.submit(new Callable[A] {
        override def call(): A = a(es).get()
      })
    }

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = p.map2(p2)(_ == _)

    //Ex 4
    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] = parList.map(_.sorted)

    //Ex 5
    def sequence_simple[A](l: List[Par[A]]): Par[List[A]] = {
      l.foldRight[Par[List[A]]](unit(List()))((h, t) => h.map2(t)(_ :: _))
    }

    def sequence_right[A](l: List[Par[A]]): Par[List[A]] = {
      l match {
        case Nil => unit(Nil)
        case hPar :: tPar => hPar.map2(fork(sequence_right(tPar)))((h, t) => h :: t)
      }
    }

    //Nice
    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) as.head.map(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](l: List[Par[A]]): Par[List[A]] = sequenceBalanced(l.toIndexedSeq).map(_.toList)

    def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs = l.map(asyncF(f))

      sequence(fbs)
    }

    //Ex 6
    def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars: List[Par[List[A]]] = l.map(asyncF((a: A) => if (f(a)) List(a) else List()))

      sequence(pars).map(_.flatten)
    }

    def reduce[A](seq: IndexedSeq[A])(f: (A, A) => A): Par[A] = fork {
      if (seq.size == 0) throw new UnsupportedOperationException("Reduce on 1 element")
      else if (seq.size == 1) unit(seq.head)
      else {
        val (l, r) = seq.splitAt(seq.length / 2)
        reduce(l)(f).map2(reduce(r)(f))(f)
      }
    }

    def fold[A, B](z: B)(seq: IndexedSeq[A])(f: A => B)(g: (B, B) => B): Par[B] = fork {
      if (seq.size == 0) unit(z)
      else if (seq.size == 1) unit(f(seq.head))
      else {
        val (l, r) = seq.splitAt(seq.length / 2)
        val lz = fold(z)(l)(f)(g)
        val rz = fold(z)(r)(f)(g)

        lz.map2(rz)(g)
      }
    }

    def sum(ints: IndexedSeq[Int]): Par[Int] = fork {
      if (ints.size <= 1) {
        unit(ints.headOption getOrElse 0)
      }
      else {
        val (l, r) = ints.splitAt(ints.length / 2)
        sum(l).map2(sum(r))(_ + _)
      }
    }

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get() == p2(e).get()

    def delay[A](fa: => Par[A]): Par[A] = (es: ExecutorService) => fa(es)
  }

  //Ex 7
  /*
    Given y.map(id) == y (1)

    To prove (y.map(g)).map(f) == y.map(f compose g)

    Substitute id for g.
    (y.map(id)).map(f) == y.map(f compose id)

    From (1) above
    y.map(f) == y.map(f)
   */


  //Ex 8 - Breaks for fixed thread pools

  //Ex 9 - For n threads wrap in n forks
}
