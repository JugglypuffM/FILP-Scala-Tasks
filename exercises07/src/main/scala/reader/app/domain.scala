package reader.app

object domain {
  case class Wallet(currency: Currency, amount: BigDecimal)

  case class Good(currency: Currency, price: BigDecimal)

  case class Transaction(currency: Currency, price: BigDecimal)

  type Exchanges = (Currency, Currency) => Double
}
