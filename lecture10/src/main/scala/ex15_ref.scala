

object ex15_ref extends App {

  Unique
  val task: IO[Unit] =
    for {
      ref <- Ref.of[IO, Int](0)
      _ <- List.fill(1000)().parTraverse(
        _ => ref.update(_ + 1)
      )
      result <- ref.get
    } yield println(result)

  task.unsafeRunSync()
  Thread.sleep(10000)


}

