import data.NonEmptyList
import org.scalatest.wordspec.AnyWordSpec
import writer.app.domain.{Good, Transaction, Wallet}
import writer.app.WriterApp.WriterService

class WriterSpec extends AnyWordSpec {
  "WriterService" should {
    "transact" in {
      assert(WriterService.transact(Good(100)).log.list == List("spent 100"))
      assert(WriterService.transact(Good(100)).value == Transaction(100))
    }

    "aggregate" in {
      assert(
        WriterService.aggregate(NonEmptyList.of(Transaction(100), Transaction(200))).log.list == List("spent total 300")
      )
      assert(WriterService.aggregate(NonEmptyList.of(Transaction(100), Transaction(200))).value == Transaction(300))
    }

    "buyAll" in {
      assert(WriterService.buyAll(Wallet(300)).log.list == List("spent 1", "spent 2", "spent 3", "spent total 6"))
      assert(WriterService.buyAll(Wallet(6)).value == Wallet(0))
    }
  }
}
