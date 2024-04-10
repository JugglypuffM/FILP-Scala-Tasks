import exercises08.RunEC
import org.scalatest.flatspec.AnyFlatSpec

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Random, Success, Try}

class RunECSpec extends AnyFlatSpec {
  trait mock {
    val taskArray = new ArrayBuffer[Runnable]()

    val counter: AtomicInteger                     = new AtomicInteger(0)
    val errRef: AtomicReference[Option[Throwable]] = new AtomicReference[Option[Throwable]](None)
    val sucRef: AtomicReference[Option[Int]]       = new AtomicReference[Option[Int]](None)

    val mockEC: ExecutionContext = new ExecutionContext {
      override def execute(runnable: Runnable): Unit = {
        taskArray.addOne(() => runnable.run())
      }

      override def reportFailure(cause: Throwable): Unit = ()
    }
  }
  "RunEC" should "runCallback" in new mock {
    val randomInt = Random.nextInt()
    RunEC.runCallback[Int](
      {
        counter.incrementAndGet()
        randomInt
      }
    ) {
      case Failure(e)     => errRef.set(Some(e))
      case Success(value) => sucRef.set(Some(value))
    }(mockEC)

    assert(counter.get() == 0)
    assert(errRef.get().isEmpty)
    assert(sucRef.get().isEmpty)

    taskArray.toList match {
      case head :: Nil => head.run()
      case _           => fail("Число переданных в ExecutionContext задач не равно 1")
    }

    assert(counter.get() == 1)
    assert(errRef.get().isEmpty)
    assert(sucRef.get().contains(randomInt))
  }

  "RunEC" should "runCallback fail" in new mock {
    val randomMessage = Random.alphanumeric.take(10).mkString
    RunEC.runCallback[Int](
      {
        throw new RuntimeException(randomMessage)
      }
    ) {
      case Failure(e)     => errRef.set(Some(e))
      case Success(value) => sucRef.set(Some(value))
    }(mockEC)

    assert(counter.get() == 0)
    assert(errRef.get().isEmpty)
    assert(sucRef.get().isEmpty)

    taskArray.toList match {
      case head :: Nil => head.run()
      case _           => fail("Число переданных в ExecutionContext задач не равно 1")
    }

    assert(counter.get() == 0)
    assert(errRef.get().exists(_.getMessage == randomMessage))
    assert(sucRef.get().isEmpty)
  }

  "RunEC" should "runReturn" in new mock {
    val randomInt                                 = Random.nextInt()
    val result: AtomicReference[Option[Try[Int]]] = new AtomicReference(None)
    val t = new Thread(
      () =>
        try {
          val r = RunEC.runReturn[Int](
            {
              counter.incrementAndGet()

              randomInt
            }
          )(mockEC)
          result.set(Some(r))
        } catch {
          case e: InterruptedException => ()
        }
    )
    t.start()

    Thread.sleep(1000)

    assert(counter.get() == 0)
    taskArray.toList match {
      case head :: Nil => head.run()
      case _           => fail("Число переданных в ExecutionContext задач не равно 1")
    }
    assert(counter.get() == 1)
    Thread.sleep(1000)
    assert(result.get().contains(Success(randomInt)))
    t.interrupt()
  }

  "RunEC" should "runReturn fail" in new mock {
    val randomMessage                             = Random.alphanumeric.take(10).mkString
    val result: AtomicReference[Option[Try[Int]]] = new AtomicReference(None)
    val t = new Thread(
      () =>
        try {
          val r = RunEC.runReturn[Int](
            {
              counter.incrementAndGet()

              throw new RuntimeException(randomMessage)
            }
          )(mockEC)
          result.set(Some(r))
        } catch {
          case e: InterruptedException => ()
        }
    )
    t.start()

    Thread.sleep(1000)

    assert(counter.get() == 0)
    taskArray.toList match {
      case head :: Nil => head.run()
      case _           => fail("Число переданных в ExecutionContext задач не равно 1")
    }
    assert(counter.get() == 1)
    Thread.sleep(1000)
    assert(result.get().exists(_.toEither.swap.exists(_.getMessage == randomMessage)))
    t.interrupt()
  }
}
