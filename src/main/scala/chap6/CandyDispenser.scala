package scala.chap6

import scala.chap6.StatePattern._

/**
 * Finite state automation for candy dispenser
 */
object CandyDispenser {
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  //Ex 11
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      _ <- State.sequence(inputs.map(i => State.modify((s: Machine) =>
        (i, s) match {
          case (_, Machine(_, 0, _)) => s
          case (Coin, Machine(false, _, _)) => s
          case (Turn, Machine(true, _, _)) => s
          case (Coin, Machine(true, candy, coins)) => Machine(locked = false, candy, coins + 1)
          case (Turn, Machine(false, candy, coins)) => Machine(locked = true, candy - 1, coins)
        })))
      s <- State.get
    } yield (s.coins, s.candies)
  }
}
