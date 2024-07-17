

object ex1_io_notation extends App {

  // Создание уже вычисленного IO
  val ioPure: IO[Int] = IO.pure(123)

  // Создание IO, которое будет вычислено (метод apply или delay)
  val ioDelay: IO[Int] = IO.delay {
    println("IO")
    123
  }
  val ioDelay2: IO[Int] = IO.delay {
    println("IO 2")
    123
  }

}
