package consacols

import consacols.Example05ProblemTry.calculate

object Example1Problem extends App {

  def calculate(a: Int, b: Int): Int =
    a / b

  println(calculate(1, 2)) // ok

  println(calculate(1, 0)) // runtime Failure
}
