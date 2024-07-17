

object ex3_io_functor {
  val functorIO: Functor[IO] = new Functor[IO] {
    override def map[A, B](fa: IO[A])(f: A => B): IO[B] =
      fa.map(f)
  }
}
