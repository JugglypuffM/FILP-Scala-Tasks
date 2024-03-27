package writer.app

import data.NonEmptyList
import typeclasses.{Monoid, Semigroup}
import writer.Writer
import typeclasses.Monad.syntax._
import typeclasses.Monoid.syntax._
import domain._

object WriterApp extends App {
  case class Logs(list: List[String])
  object Logs {
    implicit val monoid: Monoid[Logs] = new Monoid[Logs] {
      def combine(x: Logs, y: Logs): Logs =
        Logs(x.list ::: y.list)
      def empty: Logs =
        Logs(Nil)
    }
    def single(string: String): Logs =
      Logs(List(string))
  }

  object ClassicService {
    private def transact(good: Good): (Logs, Transaction) =
      (Logs.single(s"spent ${good.price}"), Transaction(good.price))

    private def aggregate(transactions: NonEmptyList[Transaction]): (Logs, Transaction) = {
      val all = transactions.reduce(Semigroup[Transaction].combine)
      (Logs.single(s"spent total ${all.price}"), all)
    }

    def buyAll(wallet: Wallet): (Logs, Wallet) = {
      val (log, transact1)  = transact(Good(1))
      val (log2, transact2) = transact(Good(2))
      val (log3, transact3) = transact(Good(3))
      val (logsAll, all)    = aggregate(NonEmptyList.of(transact1, transact2, transact3))
      (log |+| log2 |+| log3 |+| logsAll, wallet.copy(amount = wallet.amount - all.price))
    }
  }

  object WriterService {
    type WithLogs[A] = Writer[Logs, A]
    object WithLogs {
      def log(str: String): WithLogs[Unit] =
        Writer.tell(Logs.single(str))
    }

    def transact(good: Good): WithLogs[Transaction] = ???

    def aggregate(transactions: NonEmptyList[Transaction]): WithLogs[Transaction] = ???

    def buyAll(wallet: Wallet): WithLogs[Wallet] = ???
  }
}
