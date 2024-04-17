import f02_callbacks_with_returnes.multiplyWithCallback

import java.util.concurrent.Executors

object f05_callbacks_threadpool extends App {
  val threadPool = Executors.newSingleThreadExecutor()

  def multiplyWithCallback(a: BigInt, b: BigInt)(callback: BigInt => Unit): Unit = {
    val result = a * b
    threadPool.execute(
      () => callback(result)
    )
  }

  def factorial(n: BigInt)(callback: BigInt => Unit): Unit =
    if (n == 1) callback(1) else {
      multiplyWithCallback(n, 1) {
        result => factorial(n - 1)(f => multiplyWithCallback(f, result)(callback))
      }
    }

  factorial(50000)(
    res => {
      println(res)
      threadPool.shutdown()
    }
  )


}
