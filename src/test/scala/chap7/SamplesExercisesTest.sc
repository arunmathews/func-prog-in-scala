import java.util.concurrent.Executors
import scala.chap7.SampleExercises._
val l = IndexedSeq(1, 2, 3, 4)
val sum = Par.sum(l)
val pool = Executors newFixedThreadPool 10

val futSum = sum.run(pool)
futSum.get()

val sum2 = Par.reduce(l)((a, b) => a + b)
sum2.run(pool).get()
val sum3 = Par.reduce(IndexedSeq(1))((a, b) => a + b)
sum3.run(pool).get()
val max = Par.reduce(l)((a, b) => math.max(a, b))
max.run(pool).get()
val paras = IndexedSeq("We are are we", " No no  no    never   never")

val count = Par.fold(0)(paras)(sent => sent.trim.split("""\s+""").length)(_ + _)

count.run(pool).get()

val aSum = sum.map3(sum2, count)(_ + _ + _)
aSum.run(pool).get()
val pFiltered = Par.parFilter(l.toList)(_ % 2 == 0)
val filtered = pFiltered.run(pool).get()
pool.shutdown()
pool.isShutdown


val a = Par.lazyUnit(42 + 1)

val deadlockPool = Executors newFixedThreadPool 1

//Caused deadlock if executed
//Par.equal(deadlockPool)(a, Par.fork(a))

deadlockPool.shutdown()
deadlockPool.isShutdown
