import scala.concurrent.duration._
object ex13_io_async_cancel extends App {
  def printWithDelay(num: Int): IO[Unit] = {
    for {
      _ <- IO.sleep(1.second)
      _ <- IO(println(num))
      _ <- printWithDelay(num)
    } yield ()
  }.onCancel(IO(println(s"CANCELED: ${num}")))

  val task: IO[Unit] = for {
    fiber1 <- printWithDelay(1).start // Запускаем файбер 1
    fiber2 <- printWithDelay(2).start // Запускаем файбер 2
    _ <- fiber2.cancel // Отменяем файбер 2
  } yield ()

  task.unsafeRunSync()
  Thread.sleep(10000)


}

