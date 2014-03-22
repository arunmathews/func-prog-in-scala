package scala.chap7

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
 * Non blocking par implementation
 */
object NonBlockingPar {
  sealed trait Future[+A] {
    private[chap7] def apply(callback: Try[A] => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]


  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {

    //Ex 10: Change callback to use Try[A] instead of A
    def run(es: ExecutorService): Try[A] = {
      val ref = new AtomicReference[Try[A]]()
      val latch = new CountDownLatch(1)

      p(es)( (tryA: Try[A]) => {
        //println("Thread - " + Thread.currentThread().getId + ": Running callback")
        ref.set(tryA)
        latch.countDown()
      })

      latch.await()
      ref.get()
    }

    //Very nice implementation
    def map2[B, C](parB: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        new Future[C] {
          override def apply(callback: (Try[C]) => Unit): Unit = {
            var aResult: Option[Try[A]] = None
            var bResult: Option[Try[B]] = None

            def evaluate(tryA: Try[A], tryB: Try[B]): Try[C] = for {
              a <- tryA
              b <- tryB
            } yield f(a, b)
          
            val combiner = Actor[Either[Try[A], Try[B]]] (es) {
              case Left(tryA) => bResult match {
                case None => aResult = Some(tryA)
                case Some(tryB) => Par.evaluate(es)(callback(evaluate(tryA, tryB)))
              }

              case Right(tryB) => aResult match {
                case None => bResult = Some(tryB)
                case Some(tryA) => Par.evaluate(es)(callback(evaluate(tryA, tryB)))
              }
            }

            p(es)(a => combiner ! Left(a))
            parB(es)(b => combiner ! Right(b))
          }
        }
      }

    def map[B](f: A => B): Par[B] = map2(Par.unit(()))((a, _) => f(a))

    def flatMap[B](f: A => Par[B]): Par[B] = (es: ExecutorService) => {
      new Future[B] {
        override def apply(callback: (Try[B]) => Unit): Unit = p(es)((tryA: Try[A]) =>
          tryA.map((a: A) => Par.evaluate(es)(f(a)(es)(callback))))
      }
    }

    def flatMapUsingJoin[B](f: A => Par[B]): Par[B] = Par.join(p.map(f))
  }

  object Par {
    def unit[A](a: A): Par[A] = es => new Future[A] {
      override def apply(callback: (Try[A]) => Unit): Unit = {
        //println("Thread - " + Thread.currentThread().getId + ": Inside unit apply")
        callback(Success(a))
      }
    }

    def fork[A](a: => Par[A]): Par[A] = {
      //println("Thread - " + Thread.currentThread().getId + ": Going to wrap lazy Par A in Future")
      (es: ExecutorService) => new Future[A] {
          override def apply(callback: (Try[A]) => Unit): Unit = {
            //println("Thread - " + Thread.currentThread().getId + ": Inside Par fork apply")
            evaluate(es)(a(es)(callback))
            //println("Thread - " + Thread.currentThread().getId + ": Made call to evaluate")
          }
        }
    }

    def evaluate(es: ExecutorService)(run: => Unit): Unit =
      es.submit(new Callable[Unit]{
        override def call(): Unit = {
          //println("Thread - " + Thread.currentThread().getId + ": Going to run callable")
          run
        }
      })

    def lazyUnit[A](a: => A): Par[A] = {
      //println("Thread - " + Thread.currentThread().getId + ": Forking lazy computation")
      fork(unit(a))
    }

    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) as.head.map(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](l: List[Par[A]]): Par[List[A]] = {
      //println("Thread - " + Thread.currentThread().getId + ": Going to start sequencing list of Pars")
      sequenceBalanced(l.toIndexedSeq).map(_.toList)
    }

    def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = fork {
      //println("Thread - " + Thread.currentThread().getId + "Starting parMap")
      val fbs = l.map(asyncF(f))

      sequence(fbs)
    }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      (es: ExecutorService) => {
        new Future[A] {
          override def apply(callback: (Try[A]) => Unit): Unit = cond(es)((tryBool: Try[Boolean]) =>
            tryBool.map((bool: Boolean) => if (bool) evaluate(es)(t(es)(callback)) else evaluate(es)(f(es)(callback))))
        }
      }

    //Ex 11
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      (es: ExecutorService) => {
        new Future[A] {
          override def apply(callback: (Try[A]) => Unit): Unit = n(es)((tryInt: Try[Int]) =>
            tryInt.map((ind: Int) => evaluate(es)(choices(ind)(es)(callback))))
        }
      }

    def choiceUsingChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(cond.map(b => if (b) 0 else 1))(List(t, f))

    //Ex 12
    def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
      (es: ExecutorService) => {
        new Future[V] {
          override def apply(callback: (Try[V]) => Unit): Unit = key(es)((tryK: Try[K]) =>
            tryK.map((k: K) => evaluate(es)(choices(k)(es)(callback))))
        }
      }

    //Ex 13
    def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
      (es: ExecutorService) => {
        new Future[B] {
          override def apply(callback: (Try[B]) => Unit): Unit = pa(es)((tryA: Try[A]) =>
            tryA.map((a: A) => evaluate(es)(choices(a)(es)(callback))))
        }
      }

    def choiceNUsingChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      chooser(n)((x: Int) => choices(x))

    def choiceUsingChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      chooser(cond)((b: Boolean) => if (b) t else f)

    def choiceMapUsingChooser[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
      chooser(key)((k: K) => choices(k))

    //Ex 14
    def join[A](a: Par[Par[A]]): Par[A] =
      (es: ExecutorService) => {
        new Future[A] {
          override def apply(callback: (Try[A]) => Unit): Unit = a(es)((tryParA: Try[Par[A]]) =>
            tryParA.map((parA: Par[A]) => evaluate(es)(parA(es)(callback))))
        }
      }

    def joinUsingFlatMap[A](a: Par[Par[A]]): Par[A] = a.flatMap((parA: Par[A]) => parA)

  }

}
