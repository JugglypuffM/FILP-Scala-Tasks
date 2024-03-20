package exercises06.e1_list_ops

import scala.Integral.Implicits.infixIntegralOps

object Examples {
  // сделайте так, чтобы скомпилировалось
  implicit class listOps[A: Integral](list: List[A]) {
    def filterOdd: List[A] = list.filter(_.toInt % 2 != 0)
    def filterEven: List[A] = list.filter(_.toInt % 2 == 0)
  }

  List[Int](1, 2, 3).filterOdd
  List[Int](1, 2, 3).filterEven

  List[Long](1, 2, 3).filterOdd
  List[Long](1, 2, 3).filterEven

  List[BigInt](1, 2, 3).filterOdd
  List[BigInt](1, 2, 3).filterEven
}
