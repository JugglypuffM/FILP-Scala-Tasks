package lecture

import scala.annotation.tailrec

object Example05DefaultArguments extends App {

  @tailrec
  def factorialTailRec(n: Int, acc: Int = 1): Int =
    if (n <= 1) acc
    else factorialTailRec(acc * n, n - 1)

  println(factorialTailRec(10))
}
