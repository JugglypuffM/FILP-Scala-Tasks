package consacols

object Example06Either extends App {
  val eitherValue: Either[String, Int] = Right(42)

  eitherValue match {
    case Right(value)  => println(s"some $value")
    case Left(message) => println(s"nothing but $message")
  }

  val valueWithFallbackBad = if (eitherValue.isLeft) eitherValue.left.get else 0

  val transformValue    = eitherValue.map(_ * 3)
  val valueWithFallback = eitherValue.fold(_ => 0, identity)

  eitherValue.foreach(println)

  val f1: Int => Either[String, Int] = i => Right(i)
  // ....
  val fn: Int => Either[String, Int] = i => Right(i)

  val i: Either[String, Int] = Right(2)

  val result1 = i.flatMap(v => f1(v).flatMap(vv => fn(vv)))
  val result2 = for {
    r0 <- i
    r1 <- f1(r0)
    // ....
    rn <- fn(r1)
  } yield rn
}
