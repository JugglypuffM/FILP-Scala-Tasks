import scala.concurrent.duration.DurationInt

object ex7_io_start extends App {
  def printWithDelay(num: Int): IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      _ <- IO(println(num))
      _ <- printWithDelay(num)
    } yield ()

  val task: IO[Unit] = for {
    _ <- printWithDelay(1).start // Запускаем файбер 1
    fiber2 <- printWithDelay(2).start // Запускаем файбер 2
  } yield ()

  task.unsafeRunSync() // Запуск основного файбера. Основной файбер запустит файберы 1 и 2.
  Thread.sleep(10000)
}
