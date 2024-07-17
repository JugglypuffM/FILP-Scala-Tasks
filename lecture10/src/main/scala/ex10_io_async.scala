

object ex10_io_async extends App {
  val async: IO[Int] = IO.async[Int](
    (k: Either[Throwable, Int] => Unit) =>
      IO.pure { // Запускает некоторое вычисление асинхронное
        k(Right(4))
      }.as(None)
  )

  val a = async.unsafeRunSync()
  println(a)
}

