package lecture8

import java.util.concurrent.Executors

object t6_Java_Thread_Pool extends App {
  val fixedThreadPool = Executors.newCachedThreadPool()

  List.range(0, 10).foreach(
    i => fixedThreadPool.execute(
      () => task(i)
    )
  )


  Thread.sleep(10000)
}
