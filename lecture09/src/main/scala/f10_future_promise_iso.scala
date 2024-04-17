import scala.concurrent.{ExecutionContext, Future, Promise}

object f10_future_promise_iso extends App {
  val ec =  ExecutionContext.global

  def squareCallback(arg: Int, cb: Int => Unit): Unit = {
    val result = arg * arg
    ec.execute(() => cb(result))
  }

  def squareFuture(arg: Int): Future[Int] = {
    val p: Promise[Int] = Promise() // Создаем коробочку

    squareCallback(arg, result => p.success(result)) // В колбеке указываем -
    // положи в коробочку

    p.future // Конвертим Promise во Future
  }


}
