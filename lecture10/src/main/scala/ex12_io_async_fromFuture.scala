import scala.concurrent.Future
import scala.util.{Failure, Success}

object ex12_io_async_fromFuture extends App {
  def fromFuture[A](future: Future[A]): IO[A] =
    IO.async(
      (k: Either[Throwable, A] => Unit) =>
        IO.executionContext.map(
          ec => future.transform {
            case Failure(exception) => Success(k(Left(exception)))
            case Success(value) => Success(k(Right(value)))
          }(ec)
        ).as(None)

    )


}

