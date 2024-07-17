

// Пример с файлом привести
object ex18_resources extends App {

  val task = for {
    ref <- Ref.of[IO, Int](0)
    resource = Resource.make(
      acquire = ref.update(_ + 1)
    )(
      release = _ => ref.update(_ - 1)
    )

    fiber <- resource.use(
      _ => for {
        _ <- ref.get.map(println)
//        _ <- IO.raiseError(new Throwable())
      } yield ()
    ).start
//    _ <- fiber.cancel
    _ <- fiber.join

    _ <- ref.get.map(println)
  } yield ()

  task.unsafeRunSync()
}
