package lecture8

import lecture8.t1_Task.task

object t2_Threads extends App {


  val t = new Thread(
    () => task(0)
  )

  t.start()
//  t.start()

  Thread.sleep(10000) // Чтобы программа не завершилась раньше времени
}
