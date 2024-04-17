import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, Future, Promise}


object f19_future_traverse_sequence extends App {
  private val scheduler = Executors.newScheduledThreadPool(1)
  private implicit val ec = ExecutionContext.fromExecutor(scheduler)

  def asyncSleep(duration: FiniteDuration): Future[Unit] = {
    val promise = Promise[Unit]()
    scheduler.schedule(
      new Runnable {
        override def run(): Unit = promise.success()
      }, duration.toNanos, TimeUnit.NANOSECONDS
    )
    promise.future
  }

//  def calculateSquareWithAsyncDelay(i: Int): Future[Int] =
//    asyncSleep(5.seconds).map(_ => i * i)

  def calculateSquareWithAsyncDelay(i: Int): Future[Int] =
    for {
      _ <- asyncSleep(3.seconds)
    } yield  i * i

  val tasks: List[Future[Int]] = List.range(1, 10)
    .map(calculateSquareWithAsyncDelay)

  val sequenced: Future[List[Int]] = Future.sequence(tasks)

  val traversed: Future[List[Int]] = Future.traverse(List.range(1, 10))(
    calculateSquareWithAsyncDelay
  )

  val sequencedResult: List[Int] = Await.result(sequenced, 5.seconds)
  println(sequencedResult)
}
