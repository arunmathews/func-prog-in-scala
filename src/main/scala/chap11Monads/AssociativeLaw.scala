package scala.chap11Monads

import scala.chap8.ChapterSamples._
/**
 * Testing associative law for Monads
 */
object AssociativeLaw {
  case class Item(name: String, price: Double)
  case class Order(item: Item, quantity: Int)

  val genOrder: Gen[Order] = for {
    name <- Gen.stringN(3)
    price <- Gen.uniform
    quantity <- Gen.choose(1, 100)
  } yield Order(Item(name, price), quantity)

  val genItem: Gen[Item] = for {
    name <- Gen.stringN(3)
    price <- Gen.uniform
  } yield Item(name, price)

  val genOrder2: Gen[Order] = for {
    item <- genItem
    quantity <- Gen.choose(1, 100)
  } yield Order(item, quantity)

  //Ex 7
  val genOrdExp: Gen[Order] = Gen.stringN(3).flatMap(s => Gen.uniform.flatMap(p => Gen.choose(1, 100).map(q => Order(Item(s, p), q))))

  val genOrder2Exp: Gen[Order] = Gen.stringN(3).flatMap(s => Gen.uniform.map(p => Item(s, p)).flatMap(i => Gen.choose(1, 100).map(q => Order(i, q))))

  //Ex 8
  //To prove x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g)) where x is Option
  //By definition flatMap makes Option(a).f(a => Option[B]) to Option[B]
  //If x - None
  //None.flatMap(f).flatMap(g) = None = None.flatMap(a => f(a).flatMap(g))
  //If x - Some(u)
  //Let f: u => Option[V], g: v => Option[W]
  //x.flatMap(f).flatMap(g) = Some(u).flatMap(u => Some(v)).flatMap(g) = Some(v).flatMap(v => Some(w)) = Some(w)
  //x.flatMap(a => f(a).flatMap(g)) = Some(u).flatMap(a => f(a).flatMap(g)) = Some(u).flatMap(u => f(u).flatMap(g)) =
  //Some(u).flatMap(u => f(u).flatMap(v => Some(w)) = Some(u).flatMap(u => Some(v).flatMap(v => Some(w)) =
  //Some(u).flatMap(u => Some(w)) = Some(w)
  val f: String => Gen[Item] = s => Gen.uniform.map(p => Item(s, p))
  val g: Item => Gen[Order] = i => Gen.choose(1, 100).map(q => Order(i, q))
  val m1 = Gen.stringN(3).flatMap(f).flatMap(g)
  val m2 = Gen.stringN(3).flatMap(a => f(a).flatMap(g))

  //Ex 11
  //Show that x.compose(compose(f, g), h) == x.compose(f, compose(g, h)) same as x.flatMap(flatMap(f)(g))(h) == x.flatMap(f)(b => flatMap(g(b))(h))
  //x: F[A], f: A => F[B], g: B => F[C], h: C => F[D]
  //Rewrite compose in terms of flatMap
  //compose(compose(f, g), h) == compose(f, compose(g, h))
  //a => flatMap(compose(f, g)(a)(h) == a => flatMap(f(a))(compose(g, h))
  //a => flatMap((b => flatMap(f(b))(g))(a))(h) == a => flatMap(f(a))(b => flatMap(g(b))(h))
  //Now f is a function that takes A as input. So simplify
  //a => flatMap(flatMap(f(a))(g))(h) == a => flatMap(f(a))(b => flatMap(g(b))(h))
  //Let f(a) = x
  //flatMap(flatMap(x)(g))(h) == flatMap(x)(b => flatMap(g(b))(h))
  //flatMap(flatMap(x)(f))(g) == flatMap(x)(a => flatMap(f(a))(g))

  //Ex 12
  //compose(f, unit) == f
  //a => flatMap(f(a))(unit) == f
  //flatMap(x)(unit) == x
  //compose(unit, f) == f
  //a => flatMap(unit)(f) == f
  //flatMap(unit(x)(f) == f(x) for all x, f

  //Ex 15
  //x.flatMap(z => z).flatMap(z => z) == x.flatMap(a => a.flatMap(z => z))
  //join(join(x)) == x.flatMap(join)
  //join(join(x)) == join(map(x)(join))
  //join(unit(x)) == x && join(map(x)(unit)) == x
}
