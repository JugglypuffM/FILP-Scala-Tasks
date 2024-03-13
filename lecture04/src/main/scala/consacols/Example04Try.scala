package consacols

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Example04Try extends App {
  val tryValue: Try[Int] = Try(42 / StdIn.readInt())

  tryValue match {
    case Success(value)     => println(s"some $value")
    case Failure(exception) => println(s"nothing but $exception")
  }

  val valueWithFallbackBad: Int = if (tryValue.isSuccess) tryValue.get else 0

  val transformValue: Try[Int]   = tryValue.map(_ * 3)
  val valueWithFallback: Int     = tryValue.fold(_ => 0, identity)
  val valueWithRecover: Try[Int] = tryValue.recover { case e: Exception => 55 }
  val valueWithCollect: Try[Int] = tryValue.collect {
    case x if x == 2 => 3
  }
  println(valueWithCollect)
  tryValue.foreach(println)

  val f1: Int => Try[Int] = i => Success(i)
  // ....
  val fn: Int => Try[Int] = i => Success(i)

  val i: Try[Int] = Success(2)

  val result1: Try[Int] = i.flatMap(v => f1(v).flatMap(vv => fn(vv)))
  val result2: Try[Int] = for {
    r0 <- i
    r1 <- f1(r0)
    // ....
    rn <- fn(r1)
  } yield rn
}
