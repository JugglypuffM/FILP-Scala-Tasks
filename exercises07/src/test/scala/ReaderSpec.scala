import data.NonEmptyList
import org.scalatest.wordspec.AnyWordSpec
import reader.app.Currency._
import reader.app.ReaderApp.ReaderService
import reader.app.domain.{Exchanges, Good, Transaction, Wallet}

class ReaderSpec extends AnyWordSpec {
  val exchanges: Exchanges = {
    case (Dollar, Ruble)  => 75.45
    case (Ruble, Dollar)  => 0.013
    case (Euro, Ruble)    => 90.72
    case (Ruble, Euro)    => 0.011
    case (Euro, Dollar)   => 0.5
    case (Dollar, Euro)   => 0.3
    case (Yen, Ruble)     => 0.71
    case (Ruble, Yen)     => 1.41
    case (Yen, Dollar)    => 2
    case (Dollar, Yen)    => 108.78
    case (Yen, Euro)      => 0.0077
    case (Euro, Yen)      => 129.87
    case (Yen, Yen)       => 1
    case (Euro, Euro)     => 1
    case (Dollar, Dollar) => 1
    case (Ruble, Ruble)   => 1
  }

  "ReaderService" should {
    "transact" in {
      assert(
        ReaderService.transact(Dollar, Good(Euro, 10)).run(exchanges) == Transaction(Dollar, 5)
      )
    }

    "aggregate" in {
      assert(
        ReaderService
          .aggregate(Dollar, NonEmptyList.of(Transaction(Euro, 10), Transaction(Yen, 5)))
          .run(exchanges) == Transaction(Dollar, 15)
      )
    }

    "buyAll" in {
      assert(ReaderService.buyAll(Wallet(Euro, 100)).run(exchanges) == Wallet(Euro, 94.334938))
    }
  }
}
