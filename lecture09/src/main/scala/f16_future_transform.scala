import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}

object f16_future_transform extends App {
  implicit val ec = ExecutionContext.global

  val futa: Future[Int] = Future.failed(new RuntimeException("A"))

  val res = futa.transform( // Простенькая версия. map сразу ошибки в ошибку и успеха в успех
    (suc: Int) => 34,
    (err: Throwable) => new RuntimeException("B")
  )

  println {
    Await.result(res, 10.seconds)
  }
}
