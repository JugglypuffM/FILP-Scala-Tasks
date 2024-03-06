package lecture

import scala.annotation.tailrec

object Example03Recursion extends App {

  def factorial(n: Int): Int = {
    var result = 1
    var number = n
    while (number > 1) {
      result *= number
      number -= 1
    }
    result
  }

  println(factorial(10))

  def factorialRec(n: BigInt): BigInt =
    if (n <= 1) 1
    else n * factorialRec(n - 1)

//  println(factorialRec(10))
//  println(factorialRec(10000))

  /*
    factorialRec(3)
    3 * factorialRec(2)
    3 * (2 * factorialRec(1))
    3 * (2 * 1)
    3 * 2
    6
   */
  @tailrec
  def factorialTailRec(accumulator: BigInt, n: BigInt): BigInt =
    if (n <= 1) accumulator
    else factorialTailRec(accumulator * n, n - 1)

//  println(factorialTailRec(1, 10000))

  /*
    factorialTailRec(1, 5))
    factorialTailRec(1 * 5, 4))
    factorialTailRec(1 * 5 * 4, 3))
    factorialTailRec(1 * 5 * 4 * 3, 2))
    factorialTailRec(1 * 5 * 4 * 3 * 2, 1))
    120
 */
}
