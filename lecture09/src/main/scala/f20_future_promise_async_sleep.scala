import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future, Promise}

object f20_future_promise_async_sleep extends App {
  private val scheduler = Executors.newScheduledThreadPool(1)
  private implicit val ec = ExecutionContext.fromExecutor(scheduler)

  def asyncSleep(duration: FiniteDuration): Future[Unit] = {
    val promise = Promise[Unit]()

    scheduler.schedule(
      new Runnable { // () => Unit
        override def run(): Unit = promise.success()
      },
      duration.toSeconds, TimeUnit.SECONDS
    )

    promise.future
  }


  def sleepAndPrint(id: Int): Future[Unit] =
    for {
      _ <- asyncSleep(5.seconds)
      _ <- Future(println(s"print ${id}"))
    } yield ()

  List.range(1, 20).map(
    i => sleepAndPrint(i)
  )


  Thread.sleep(10000)

}
