

object ex1_io_notation_2 extends App {
  implicit val runtime: IORuntime = IORuntime.global // default runtime (interpreter)

  val ioPrint60: IO[Unit] =
    IO(10)
    .map(_ * 2)
    .map(_ * 3)
    .map(i => println(i))

  val ioPrint60for: IO[Unit] =
    for {
      initial <- IO(10)
      mul2 = initial * 2
      mul6 = mul2 * 3
    } yield println(mul6)
//
  ioPrint60for.unsafeRunSync()(runtime)
  ioPrint60for.unsafeRunSync()

}
