package exercises06.e1_list_ops

import scala.Integral.Implicits.infixIntegralOps

class ListOps[A: Integral](list: List[A]) {}

object Examples {
  // сделайте так, чтобы скомпилировалось
  def listOps[A: Integral](list: List[A]): ListOps[A] = new ListOps[A](list)

  List[Int](1, 2, 3).filterOdd
  List[Int](1, 2, 3).filterEven

  List[Long](1, 2, 3).filterOdd
  List[Long](1, 2, 3).filterEven

  List[BigInt](1, 2, 3).filterOdd
  List[BigInt](1, 2, 3).filterEven
}
