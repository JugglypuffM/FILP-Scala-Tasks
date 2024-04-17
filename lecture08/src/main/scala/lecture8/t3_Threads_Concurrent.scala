package lecture8

import lecture8.t1_Task.task

object t3_Threads_Concurrent extends App {


  val t1 = new Thread(
    () => task(1)
  )

  val t2 = new Thread(
    () => task(10)
  )

  t1.start()
  t2.start()

  Thread.sleep(10000) // Чтобы программа не завершилась раньше времени
}
