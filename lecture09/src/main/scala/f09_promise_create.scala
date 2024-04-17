import scala.concurrent.{Future, Promise}

object f09_promise_create extends App {

  val p: Promise[Int] = Promise()

  p.success(10) // Можно положить значение

  p.failure(new RuntimeException("Exception")) // Или ошибку!


}
