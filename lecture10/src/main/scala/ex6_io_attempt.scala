

object ex6_io_attempt extends App {
  def getNumber: IO[Int] = IO.raiseError(new RuntimeException())

  val attempted: IO[Either[Throwable, Int]] = getNumber.attempt

  attempted.map(
    (e: Either[Throwable, Int]) => println(e)
  ).unsafeRunSync()

}

