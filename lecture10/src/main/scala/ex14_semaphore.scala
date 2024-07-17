

object ex14_semaphore extends App {
  var number: Int = 0


  val task: IO[Unit] =
    for {
      semaphore <- Semaphore[IO](1)
      _ <- List.fill(1000)().parTraverse(
        _ => for {
//          _ <- semaphore.acquire
          _ <- IO { number += 1 }
//          _ <- semaphore.release
        } yield ()
      )
    } yield println(number)

  task.unsafeRunSync()
  Thread.sleep(10000)


}

