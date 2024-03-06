package consacols

object Example07ProblemEither extends App {
  def calculate(a: Int, b: Int): Either[String, Int] = b match {
    case 0 => Left("Ц ц ц")
    case _ => Right(a / b)
  }

  println(calculate(1, 2)) // ok

  println(calculate(1, 0)) // Left(message)
}
