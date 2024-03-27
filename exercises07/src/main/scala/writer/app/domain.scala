package writer.app

import typeclasses.{Monoid, Semigroup}

object domain {
  case class Good(price: BigDecimal)
  case class Wallet(amount: BigDecimal)
  case class Transaction(price: BigDecimal)
  object Transaction {
    implicit val semigroup: Semigroup[Transaction] = ???
  }
}
