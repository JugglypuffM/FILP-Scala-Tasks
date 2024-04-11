package lecture8

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicReference

object t9_Take_Value_From_Thread_Var extends App {
  def calculateSin(v: Double, callback: Double => Unit): Unit = {
    Thread.sleep(5000) // Долго считает поток синус!
    val result = Math.sin(v)
    callback(result)
  }

  val threadPool = Executors.newFixedThreadPool(10)

  var result: AtomicReference[Double] = new AtomicReference[Double](0)

  threadPool.execute(
    () => calculateSin(Math.PI / 2, res => {
      result.set(res)
      result.synchronized {
        result.notifyAll()
      }
    })
  )

  result.synchronized {
    println("WAIT")
    result.wait()
    println("WAIT DONE")
  }
  println(result)

  Thread.sleep(10000)
}
