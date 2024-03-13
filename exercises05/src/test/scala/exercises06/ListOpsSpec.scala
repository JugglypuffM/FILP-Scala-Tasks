package exercises06

import org.scalatest.wordspec.AnyWordSpec

class ListOpsSpec extends AnyWordSpec {
  "List" should {
    "have methods filterOdd and filterEven" in {
      import exercises06.e1_list_ops.Examples.listOps

      assert(List[Int](1, 2, 3).filterOdd == List(1, 3))
      assert(List[Int](1, 2, 3).filterEven == List(2))

      assert(List[Long](1, 2, 3).filterOdd == List(1, 3))
      assert(List[Long](1, 2, 3).filterEven == List(2))

      assert(List[BigInt](1, 2, 3).filterOdd == List[BigInt](1, 3))
      assert(List[BigInt](1, 2, 3).filterEven == List[BigInt](2))
    }
  }
}
