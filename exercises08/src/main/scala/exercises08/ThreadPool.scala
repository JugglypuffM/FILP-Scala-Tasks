package exercises08

import java.io.Closeable
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

object ThreadPool {

  private class ThreadPool(maxThreads: Int, minThreads: Int, ttl: FiniteDuration)(factory: MyThreadFactory) {
    sealed trait ThreadState

    // Поток готов к взятию и выполнению новой задачи
    case object FreeForWork extends ThreadState

    // Поток занят выполнением задачи
    case object Busy extends ThreadState

    // Поток был навсегда остановлен
    case object Terminated extends ThreadState

    // Очередь задач.
    // Worker потоки читают эту очередь, берут из нее задачи, выполнюят их.
    // Следует использовать Thread safe структуру данных, так как несколько потоков будут брать
    // из очереди объекты, тем самым мутируя ее.
    val queue: java.util.concurrent.BlockingQueue[() => Unit] = new LinkedBlockingQueue[() => Unit]()

    private val workerThreads = new ArrayBuffer[WorkerThread]()

    private class WorkerThread {
      def interrupt(): Unit = underlying.get().foreach(_.interrupt())

      var underlying: AtomicReference[Option[Thread]] = new AtomicReference[Option[Thread]](None)
      /*
      Алгоритм рабоего потока. Заключается в следующем:
      1. Поток достает из очереди пула (queue) задачу, блокируется, пока не достанет значение.
       При этом используется таймаут ttl.
      2. Если сработает таймаут (в течение ttl времени не будет получено значение из очереди),
      поток прекращает свою работу, функция завершается. Рекомендуется использовать метод poll с указанием таймаута.
      3. Если в течение ttl времени поток успевает получить задачу, он исполняет ее. Пока поток выполняет задачу,
      его состояние должно быть Busy. Как только задача выполнена, состояние становится FreeForWork.
      4. Поток возвращается к пункту 1.
       */
      @tailrec
      private final def workerThreadAlg(): Unit = {
        Try(queue.poll(ttl.length, ttl.unit)).fold(
          _ => threadState.set(Terminated),
          task => {
            threadState.set(Busy)
            task()
            threadState.set(FreeForWork)
          }
        )
        if (threadState.get() == Terminated) return
        workerThreadAlg()
      }

      val threadState: AtomicReference[ThreadState] = new AtomicReference[ThreadState](FreeForWork)

      // Функция запускает поток, который выполняет workerThreadAlg в вечной рекурсии.
      def start(): Unit = factory.startThread(() => workerThreadAlg()).start()
    }

    def startWorkerThread(): Unit = {
      val t: WorkerThread = new WorkerThread

      workerThreads.addOne(t)
      t.start()
    }

    // ThreadPool должен уметь расширяться. Если все потоки заняты (см. threadState) и есть место (сейчас активных потоков меньше чем maxThreads),
    // можно создать новый worker поток.
    // Может возникнуть конкуренция, если несколько потоков проверят какое-нибудь условие, войдут в блок создания потока,
    // создадут потоков больше чем maxThreads. Для избежания этого можно воспользоваться блоком synchronized или ReentrantLock.
    def tryToExpandThreads(): Unit = synchronized {
      val busy   = workerThreads.count(_.threadState.get() == Busy)
      val active = workerThreads.length - workerThreads.count(_.threadState.get() == Terminated)
      if ((active == busy) && (active < maxThreads)) startWorkerThread()
    }

    def execute(task: () => Unit): Unit = {
      tryToExpandThreads()
      queue.put(task)
    }

    // Блок создает начальный набор потоков равный minSize.
    List.range(0, minThreads).foreach(_ => startWorkerThread())

    def shutdown(): Unit = {
      workerThreads.foreach(_.interrupt())
    }

  }

  def makeThreadPoolEC(maxThreads: Int, minThreads: Int, ttl: FiniteDuration)(
      f: MyThreadFactory
  ): ExecutionContext with Closeable = {

    val tp = new ThreadPool(maxThreads, minThreads, ttl)(f)

    new ExecutionContext with Closeable {
      override def execute(runnable: Runnable): Unit = tp.execute(() => runnable.run())

      override def reportFailure(cause: Throwable): Unit = ()

      override def close(): Unit = tp.shutdown()
    }
  }
}
