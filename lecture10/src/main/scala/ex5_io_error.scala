

object ex5_io_error extends App {
  def getNumber: IO[Int] = IO(12)

  // Выбрасывание ошибок
  def makeDivide(a: Int, b: Int): IO[Double] =
    if(b == 0) IO.raiseError(new RuntimeException("Div by zero")) else
      IO {
        a.toDouble / b
      }

  val task: IO[Double] =
    for {
      num <- getNumber
      div <- makeDivide(num, 0)
    } yield div

  task.unsafeRunSync()
  // Обработка ошибок

  val task2: IO[Double] =
    task.handleErrorWith {
      error => IO(-1)
    }

//  val result =task.unsafeRunSync()
  val result = task2.unsafeRunSync()
  println(result)
}
