import scala.chap7.Actor
import scala.chap7.NonBlockingPar._
import java.util.concurrent.Executors
import scala.util.{Success, Failure, Try}
//println("Thread - " + Thread.currentThread().getId + ": Main")
val unitP = Par.unit(5)
val forkP = Par.fork(unitP)
val threadPool = Executors newFixedThreadPool 2
//val output = forkP.run(threadPool)
//println("Thread - " + Thread.currentThread().getId + ": Main continues")
val echoer = Actor[String](threadPool) {
  msg => println(s"Got message: '$msg'")
}
echoer ! "Hi Hi"
echoer ! "Ok ok"


val parMap = Par.parMap((1 to 1000).toList)((x: Int) => {
  eval(math.sqrt(x))
})

val success = runTryList(parMap)





val parMapFail = Par.parMap((1 to 1000).toList)((x: Int) =>
  eval(x / 0))

val resultFail = runTryList(parMapFail)

def runTryList[A](pars: Par[List[Try[A]]]): Try[List[A]] = for {
  trys <- pars.run(threadPool)
  tryList <- sequence(trys)
} yield tryList

def eval[A](t: => A): Try[A] =
  try {
    Success(t)
  }
  catch {
    case ex: Any => Failure(ex)
  }

//Compose
def compose[A, B, C](tryA: Try[A], tryB: Try[B], f: (A, B) => C): Try[C] = for {
  a <- tryA
  b <- tryB
} yield f(a, b)

//Sequence
def sequence[A](trys: List[Try[A]]): Try[List[A]] =
 trys.foldLeft(Try(List[A]()))((tla: Try[List[A]], ta: Try[A]) =>
   compose(tla, ta, (la: List[A], a: A) => la :+ a))

threadPool.shutdown()
threadPool.isShutdown
