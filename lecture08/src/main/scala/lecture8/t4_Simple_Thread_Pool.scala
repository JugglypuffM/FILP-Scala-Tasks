package lecture8

import java.util.concurrent.LinkedBlockingQueue
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.util.Try

object t4_Simple_Thread_Pool {

  class SimpleThreadPool(numThreads: Int) extends ExecutionContext {
    val queue = new LinkedBlockingQueue[() => Unit]() // Очередь с приходящими задачами

    @tailrec
    final def workOnQueue(): Unit = {
      val task = queue.take()

      Try(task()).fold(
        err => reportFailure(err),
        suc => ()
      ) // Чтобы обработка очереди не прекращалась в случае завершения задачи ошибкой

      workOnQueue()
    }

    // Создаем потоки и приказываем им читать очередь
    List.fill(numThreads) {
      new Thread(() => workOnQueue())
    }.foreach(_.start())

    override def execute(runnable: Runnable): Unit =
      queue.put(
        () => runnable.run()
      )

    override def reportFailure(cause: Throwable): Unit =
      println(s"FAILURE: ${cause} in thread ${Thread.currentThread()}")
  }

}
