import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

object f06_callbacks_async extends App {
  val scheduler = Executors.newScheduledThreadPool(1)

  def asyncSleep(delay: FiniteDuration)(callback: () => Unit): Unit = {
    scheduler.schedule(
      new Runnable {
        override def run(): Unit = callback()
      }, delay.toSeconds, TimeUnit.SECONDS
    )
  }

  def asyncPrint(s: String)(callback: () => Unit): Unit = {
    println(s)
    scheduler.execute(() => callback())
  }

  def printWithSomeDelay(id: Int): Unit =
    asyncPrint(s"Start $id") {
      () => asyncSleep(5.seconds) {
        () => asyncPrint(s"End $id") {
          () => ()
        }
      }
    }

  List.range(1, 10).foreach(
    id => printWithSomeDelay(id)
  )

}
