import scala.concurrent.duration.DurationInt

object ex9_io_join_multiple extends App {
  def resultWithDelay(num: Int): IO[Int] =
    for {
      _ <- IO.sleep(5.second)
    } yield num * num

  val task: IO[Int] = for {
    fiber1 <- resultWithDelay(1).start
    fiber2 <- resultWithDelay(2).start
    fiber3 <- resultWithDelay(3).start


    fiber1Outcome <- fiber1.join
    fiber1Result <- fiber1Outcome match {
      case Outcome.Succeeded(fa: IO[Int]) => fa
      case Outcome.Errored(e: Throwable) => ???
      case Outcome.Canceled() => ???
    }
    fiber2Outcome <- fiber2.join
    fiber2Result <- fiber2Outcome match {
      case Outcome.Succeeded(fa: IO[Int]) => fa
      case Outcome.Errored(e: Throwable) => ???
      case Outcome.Canceled() => ???
    }
    fiber3Outcome <- fiber3.join
    fiber3Result <- fiber3Outcome match {
      case Outcome.Succeeded(fa: IO[Int]) => fa
      case Outcome.Errored(e: Throwable) => ???
      case Outcome.Canceled() => ???
    }

  } yield fiber1Result * fiber2Result * fiber3Result // 1 * 4 * 9 = 36

  println(task.unsafeRunSync()) // Запуск основного файбера. Основной файбер запустит файбер 1 и будет ждать, пока он не завершится
}
