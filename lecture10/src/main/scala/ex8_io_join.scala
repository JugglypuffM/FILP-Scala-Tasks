import scala.concurrent.duration.DurationInt

object ex8_io_join extends App {
  def resultWithDelay(num: Int): IO[Int] =
    for {
      _ <- IO.sleep(5.second)
    } yield num * num

  val task: IO[Int] = for {
    fiber1 <- resultWithDelay(1).start
    fiber2 <- resultWithDelay(2).start

    fiber1Outcome <- fiber1.join
    fiber1Outcome <- fiber2.join

    fiber1Result <- fiber1Outcome match {
      case Outcome.Succeeded(fa: IO[Int]) => fa
      case Outcome.Errored(e: Throwable) => ???
      case Outcome.Canceled() => ???
    }
  } yield fiber1Result

  println(task.unsafeRunSync()) // Запуск основного файбера. Основной файбер запустит файбер 1 и будет ждать, пока он не завершится
}
