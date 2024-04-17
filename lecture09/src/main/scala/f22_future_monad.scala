import scala.concurrent.Future
import scala.concurrent.ExecutionContext


object f22_future_monad {
  trait Monad[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  def monadFuture(ec: ExecutionContext): Monad[Future] = new Monad[Future] {

    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = 
      fa.map(e => f(e))(ec)

    override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
      fa.flatMap(f)(ec)

  }
}
