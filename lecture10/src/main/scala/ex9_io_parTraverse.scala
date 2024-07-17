import scala.concurrent.duration.DurationInt

object ex9_io_parTraverse extends App {
  def resultWithDelay(num: Int): IO[Int] =
    for {
      _ <- IO.sleep(5.second)
    } yield num * num

  val ioListInt: IO[List[Int]] =
    List.range(1, 1000).parTraverse(
      i => resultWithDelay(i)
    )

  val ioInt: IO[Int] =
    ioListInt.map(_.sum)

  println(ioInt.unsafeRunSync()) // Запуск основного файбера. Основной файбер запустит файбер 1 и будет ждать, пока он не завершится
}
