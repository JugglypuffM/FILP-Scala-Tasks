import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}

object f18_future_transform_3 extends App {
  implicit val ec = ExecutionContext.global

  def division(a: Int, b: Int): Future[Int] =
    if(b == 0) Future.failed(new RuntimeException("Div by zero"))
    else Future.successful(a / b)

  val result = division(3,0).transformWith {
    case Failure(exception) => Future(-1)
    case Success(value) => Future.successful(value)
  }

  println {
    Await.result(result, 10.seconds)
  }


}
