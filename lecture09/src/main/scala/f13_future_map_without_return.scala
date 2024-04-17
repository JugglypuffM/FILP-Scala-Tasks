import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

object f13_future_map_without_return extends App {
  implicit val ec = ExecutionContext.global

  val futa: Future[Int] = Future.successful(10)

  futa.map(
    result => println(result)
  )

}
