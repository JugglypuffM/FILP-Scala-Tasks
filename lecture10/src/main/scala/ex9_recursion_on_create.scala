

object ex9_recursion_on_create extends App {
  def createIORec(i: Int): IO[Int] =
    for {
      r1 <- createIORec(i)
      r2 <- createIORec(i * i)
    } yield r1 + r2

  createIORec(1)

//  def sleepAndDie: IO[Unit] =
//    for {
//      _ <- IO.sleep(4.seconds)
//      _ <- createIORec(1)
//    } yield ()
//
//  sleepAndDie.unsafeRunSync()
}
