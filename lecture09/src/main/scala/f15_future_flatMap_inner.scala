import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

object f15_future_flatMap_inner extends App {

  def flatMap[A, B](future: Future[A])(f: A => Future[B]): Future[B] = {
    val resultPromise: Promise[B] = Promise()

    future.map(
      a => {
        val midFuture: Future[B] = f(a)
        midFuture.map(resultPromise.success)
      }
    )

    resultPromise.future
  }
}
