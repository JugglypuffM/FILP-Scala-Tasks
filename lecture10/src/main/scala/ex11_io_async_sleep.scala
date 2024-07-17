import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

object ex11_io_async_sleep extends App {
  val scheduler = Executors.newScheduledThreadPool(1)


  // Примерно то же самое что IO.sleep
  def sleep(dura: FiniteDuration): IO[Unit] = IO.async[Unit](
      (k: Either[Throwable, Unit] => Unit) =>
      IO.pure { // Запускает некоторое вычисление асинхронное
        scheduler.schedule(
          new Runnable {
            override def run(): Unit = k(Right())
          },
          dura.toNanos,
          TimeUnit.NANOSECONDS
        )
      }.as(None)
  )

  def sleepAndPrint(i: Int): IO[Unit] =
    for {
      _ <- sleep(5.seconds)
      _ <- IO(println(i))
    } yield ()


}

