import scala.chap5.ChapterSamples._

val testStream = Stream(1, 2, 3, 4, 5)









testStream.toListNonTailRecursive

testStream.toListTailRecursive

testStream.toList

testStream.take(2).toList

testStream.takeWhile(_ % 2 != 0).toList

testStream.exists(_ % 2 == 0)

val newStream = Stream(2, 4, 6, 8)







newStream.forAll(_ % 2 == 0)

newStream.forEach(println(_))




testStream.takeWhileFoldRight(_ % 2 != 0).toList

testStream.map(_ * 5).take(2).toList

testStream.filter(_ % 2 == 0).take(2).toList

testStream.append(Stream(10,11)).take(6).toList

testStream.map(_ + 10).filter(_ % 2 == 0).take(2).toList

testStream.find(_ % 4 == 0)

val ones: Stream[Int] = Stream.cons(1, ones)




ones.take(4).toList

ones.existsFoldRight(_ % 2 != 0)

ones.map(_ + 1).filter(_ % 2 == 0).take(2).toList

val twos = Stream.constant(2)



twos.take(2).toList

twos.map(_ % 2 == 0).take(3).toList

val from2 = Stream.from(2)



from2.take(4).toList

fibs.take(9).toList

fibsUnfold.take(9).toList

fromUnfold(2).take(4).toList

testStream.takeUnfold(3).toList

testStream.takeWhileUnfold(_ % 2 != 0).toList

testStream.zip(Stream('a', 'b', 'c', 'd')).take(2).toList

val testStream2 = Stream('b', 'f')

testStream.zipAll(testStream2).take(6).toList

startsWith(testStream, testStream2)
startsWith(testStream, Stream(1, 2, 3, 4, 5))

testStream.tails.take(2).toList.map(_.take(2).toList)

hasSubsequence(testStream, testStream2)

hasSubsequence(testStream, Stream(3, 4))

testStream.scanRight(0)(_+_).take(4).toList

