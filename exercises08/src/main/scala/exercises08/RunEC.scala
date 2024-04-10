package exercises08

import java.util.concurrent.ArrayBlockingQueue
import scala.concurrent.ExecutionContext
import scala.util.Try

object RunEC {
  def runCallback[T](task: => T)(cb: Try[T] => Unit)(ec: ExecutionContext): Unit = ???
  def runReturn[T](task: => T)(ec: ExecutionContext): Try[T]                     = ???

}
