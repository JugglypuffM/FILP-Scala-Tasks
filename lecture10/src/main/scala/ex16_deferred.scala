import scala.concurrent.duration.DurationInt

object ex16_deferred extends App {

  def fillAfter5Seconds(d: Deferred[IO, Int]): IO[Unit] =
    for {
      _ <- IO.sleep(5.seconds)
      _ <- d.complete(1)
    } yield ()

  val task =
    for {
      deferred <- IO.deferred[Int]
      _ <- fillAfter5Seconds(deferred)
      result <- deferred.get
    } yield println(result)

  task.unsafeRunSync()

}
