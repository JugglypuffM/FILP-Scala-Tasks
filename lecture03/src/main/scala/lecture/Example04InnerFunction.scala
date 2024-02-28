package lecture

import scala.annotation.tailrec

object Example04InnerFunction extends App {

  @tailrec
  def factorialTailRec(n: Int, acc: Int): Int =
    if (n <= 1) acc
    else factorialTailRec(acc * n, n - 1)


  println(factorialTailRec(10, 1))



  def factorialTailRecInner(n: Int): Int = {
    @tailrec
    def f(n: Int, acc: Int): Int = {
      if (n <= 1) acc
      else f(acc * n, n - 1)
    }
    f(n, 1)
  }


//  println(factorialTailRec(10, 1))
//  println(factorialTailRecInner(10))
}
