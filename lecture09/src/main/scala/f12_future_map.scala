import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

object f12_future_map extends App {
  implicit val ec = ExecutionContext.global

  val futa: Future[Int] = Future.successful(10)


  val futaMapped: Future[Int] =
    futa
      .map(_ * 2)(ec)
      .map(_ + 1)(ec)
      .map(_ * 2)

  val result: Int = Await.result(futaMapped, 10.seconds)

  println(result) // 42

}
