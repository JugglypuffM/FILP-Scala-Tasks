package consacols

import scala.util.Try

object Example05ProblemTry extends App {
  def calculate(a: Int, b: Int): Try[Int] =
    Try(a / b)

  println(calculate(1, 2)) // ok

  println(calculate(1, 0)) // runtime Failure
}
