package scala.chap11MonadsRev

import scala.chap8.ChapterSamples.Gen

/**
 * Laws governing the monad interface
 */
object MonadLaws {
  case class Order(item: Item, quantity: Int)
  case class Item(name: String, price: Double)

  //Generate everything in one go
  val genOrder: Gen[Order] = for {
    name <- Gen.stringN(3)
    price <- Gen.uniform.map(_ * 10)
    quantity <- Gen.choose(1, 100)
  } yield Order(Item(name, price), quantity)

  val genItem: Gen[Item] = for {
    name <- Gen.stringN(3)
    price <- Gen.uniform.map(_ * 10)
  } yield Item(name, price)

  val genOrder2: Gen[Order] = for {
    item <- genItem
    quantity <- Gen.choose(1, 100)
  } yield Order(item, quantity)


}
