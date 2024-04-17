
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}

object f17_future_transform_2 extends App {
  implicit val ec = ExecutionContext.global

  val futa: Future[Int] = Future.failed(new RuntimeException())

  val result = futa.transform { // Более жесткая версия, позволяет хендлить ошибки
    case Failure(exception) => Success(0)
    case Success(value) => Success(value)
  }

  println {
    Await.result(result, 10.seconds)
  }

}
