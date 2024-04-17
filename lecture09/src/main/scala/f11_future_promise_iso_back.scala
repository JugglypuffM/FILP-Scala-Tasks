import scala.concurrent.{ExecutionContext, Future, Promise}

object f11_future_promise_iso_back extends App {
  val ec = ExecutionContext.global

  def multiplyCallback(arg: Int, cb: Int => Unit): Unit =
    multiplyFuture(arg)
      .map(
        (r: Int) => cb(r) // Прицепляем к Future некоторый Callback
      )(ec)

  def multiplyFuture(arg: Int): Future[Int] = {
    Future(arg * arg)(ec)
  }


}
