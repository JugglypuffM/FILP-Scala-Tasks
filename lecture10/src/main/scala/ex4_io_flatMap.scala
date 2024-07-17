import java.util.UUID

object ex4_io_flatMap extends App {
  def randomUUID: IO[UUID] = IO.randomUUID
  def printConsole(value: String): IO[Unit] = IO.println(value)

  val task: IO[Unit] =
    for {
      uuid <- randomUUID
      _ <- printConsole(uuid.toString)
    } yield ()

  task.unsafeRunSync()
}
