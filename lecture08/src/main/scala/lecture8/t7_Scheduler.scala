package lecture8

import java.util.concurrent.{Executors, TimeUnit}

object t7_Scheduler extends App {
  val fixedThreadPool = Executors.newScheduledThreadPool(1)


  List.range(1, 10).foreach(
    i => {
      fixedThreadPool.schedule(
        new Runnable {
          override def run(): Unit = println(i)
        },
        5, TimeUnit.SECONDS
      )
    }
  )


  Thread.sleep(10000)
}
