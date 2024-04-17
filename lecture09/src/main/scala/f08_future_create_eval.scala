import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.Await
import scala.concurrent.duration._

object f08_future_create_eval extends App {
  val ec: ExecutionContext = ExecutionContext.fromExecutor(
    Executors.newFixedThreadPool(10)
  )


  val f2: Future[Double] = Future(Math.sin(10))(ec)


  val result = Await.result(f2, 10.seconds)
  println(result)
}
