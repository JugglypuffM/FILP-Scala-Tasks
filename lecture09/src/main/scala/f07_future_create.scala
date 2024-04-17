import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object f07_future_create extends App {

  val f1: Future[Int] = Future.successful(10)

  val f2: Future[Double] = Future.failed(new Throwable())
}
