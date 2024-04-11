package reader.app

import data.NonEmptyList
import reader.Reader
import reader.app.Currency._
import reader.app.domain._
import typeclasses.Monad.syntax._

object ReaderApp extends App {
  val exchanges: Exchanges = {
    case (Dollar, Ruble)  => 75.45
    case (Ruble, Dollar)  => 0.013
    case (Euro, Ruble)    => 90.72
    case (Ruble, Euro)    => 0.011
    case (Euro, Dollar)   => 1.2
    case (Dollar, Euro)   => 0.83
    case (Yen, Ruble)     => 0.71
    case (Ruble, Yen)     => 1.41
    case (Yen, Dollar)    => 0.0092
    case (Dollar, Yen)    => 108.78
    case (Yen, Euro)      => 0.0077
    case (Euro, Yen)      => 129.87
    case (Yen, Yen)       => 1
    case (Euro, Euro)     => 1
    case (Dollar, Dollar) => 1
    case (Ruble, Ruble)   => 1
  }

  object ClassicService {
    private def transact(currency: Currency, good: Good)(exchanges: Exchanges): Transaction =
      Transaction(currency, good.price * exchanges(good.currency, currency))

    private def aggregate(currency: Currency, transactions: NonEmptyList[Transaction])(
        exchanges: Exchanges
    ): Transaction =
      transactions.reduce { (acc, cur) =>
        acc.copy(price = acc.price + (cur.price * exchanges(cur.currency, currency)))
      }

    def buyAll(wallet: Wallet): Wallet = {
      val transact1 = transact(Euro, Good(Dollar, 1))(exchanges)
      val transact2 = transact(Yen, Good(Dollar, 2))(exchanges)
      val transact3 = transact(Ruble, Good(Dollar, 3))(exchanges)
      val transact4 = transact(Dollar, Good(Dollar, 4))(exchanges)
      val all       = aggregate(wallet.currency, NonEmptyList.of(transact1, transact2, transact3, transact4))(exchanges)
      wallet.copy(amount = wallet.amount - all.price)
    }
  }

  object ReaderService {
    def transact(currency: Currency, good: Good): Reader[Exchanges, Transaction] =
      Reader(exc => Transaction(currency, good.price * exc(good.currency, currency)))

    def aggregate(currency: Currency, transactions: NonEmptyList[Transaction]): Reader[Exchanges, Transaction] =
      Reader(exc =>
        transactions
          .map(t => Transaction(currency, t.price * exc(t.currency, currency)))
          .reduce((t1, t2) => Transaction(currency, t1.price + t2.price))
      )

    def buyAll(wallet: Wallet): Reader[Exchanges, Wallet] =
      for {
        transact1 <- transact(Euro, Good(Dollar, 1))
        transact2 <- transact(Yen, Good(Dollar, 2))
        transact3 <- transact(Ruble, Good(Dollar, 3))
        transact4 <- transact(Dollar, Good(Dollar, 4))
        all       <- aggregate(wallet.currency, NonEmptyList.of(transact1, transact2, transact3, transact4))
      } yield wallet.copy(amount = wallet.amount - all.price)
  }
}
