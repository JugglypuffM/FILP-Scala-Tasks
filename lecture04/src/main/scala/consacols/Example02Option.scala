package consacols

object Example02Option extends App {

  abstract class MyOption[+A] {
    def get: A
    def isEmpty: Boolean   = this eq MyNone
    def isDefined: Boolean = !isEmpty

    def map[B](f: A => B): MyOption[B] =
      if (isEmpty) MyNone else MySome(f(this.get))

    def flatMap[B](f: A => MyOption[B]): MyOption[B] =
      if (isEmpty) MyNone else f(this.get)
  }

  case class MySome[+A](value: A) extends MyOption[A] {
    override def get: A = value
  }

  case object MyNone extends MyOption[Nothing] {
    override def get: Nothing = throw new NoSuchElementException
  }

  val func: Int => String            = _.toString
  val func2: Int => MyOption[String] = i => if (i > 2) MySome(i.toString) else MyNone

  val myOpt: MyOption[Int]      = MySome(1)
  val newOpt: MyOption[String]  = if (myOpt.isEmpty) MyNone else MySome(func(myOpt.get)) // myOpt.map(func)
  val newOpt2: MyOption[String] = if (myOpt.isEmpty) MyNone else func2(myOpt.get) // myOpt.flatMap(func2)

  val optionalValue: Option[Int] = None

  // не делайте так
  val a: Option[Int] = optionalValue match {
    case Some(value) => Some(value * 3)
    case None        => None
  }

  // и так тоже не надо
  val valueWithFallbackBad: Int =
    if (optionalValue.isDefined) optionalValue.get else 0

  val transformValue: Option[Int]       = optionalValue.map(_ * 3)
  val transformValue2: Option[Int]      = optionalValue.flatMap(x => if (x == 0) None else Some(3 / x))
  val valueWithFallback: Int            = optionalValue.getOrElse(0)
  val valueWithFallbackWithF: Int       = optionalValue.fold(0)(_ + 2)
  val valueWithFallbackOpt: Option[Int] = optionalValue.orElse(Some(0))

  val exists: Boolean = optionalValue.exists(_ > 42)
  val forAll: Boolean = optionalValue.forall(_ > 42)

  optionalValue.foreach(println)

  val f1: Int => Option[Int] = i => Some(i)
// ....
  val fn: Int => Option[Int] = i => Some(i)

  val i: Option[Int] = Some(2)

  val result1: Option[Int] = i.flatMap(v => f1(v).flatMap(vv => fn(vv)))

  val result2: Option[Int] = for {
    r0 <- i
    r1 <- f1(r0)
    // ....
    rn <- fn(r1)
  } yield rn
}
