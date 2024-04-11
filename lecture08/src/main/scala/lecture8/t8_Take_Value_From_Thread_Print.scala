package lecture8

import java.util.concurrent.Executors

object t8_Take_Value_From_Thread_Print extends App {
  def calculateSin(v: Double, callback: Double => Unit): Unit = {
    Thread.sleep(5000) // Долго считает поток синус!
    val result = Math.sin(v)
    callback(result)
  }

  val threadPool = Executors.newFixedThreadPool(10)

  List.range(1, 10).foreach(
    i => threadPool.execute(
      () => calculateSin(Math.PI / i, res => println(res))
    )
  )

  Thread.sleep(10000)
}
