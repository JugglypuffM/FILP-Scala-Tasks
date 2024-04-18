package exercises08

import java.util.concurrent.ArrayBlockingQueue
import scala.concurrent.ExecutionContext
import scala.util.Try

object RunEC {
  def runCallback[T](task: => T)(cb: Try[T] => Unit)(ec: ExecutionContext): Unit = ec.execute(() => cb(Try(task)))
  def runReturn[T](task: => T)(ec: ExecutionContext): Try[T] = {
    val queue = new ArrayBlockingQueue[Try[T]](1)
    runCallback(task)(queue.put)(ec)
    queue.take()
  }

}
