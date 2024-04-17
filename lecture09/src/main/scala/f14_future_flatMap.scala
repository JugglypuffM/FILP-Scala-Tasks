import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

object f14_future_flatMap extends App {

  def asyncPrint(s: String): Future[Unit] =
    Future(println(s))

  val future: Future[Unit] = for {
    _ <- asyncPrint("1")
    _ <- asyncPrint("2")
    _ <- asyncPrint("3")
    _ <- asyncPrint("4")
  } yield ()




}
