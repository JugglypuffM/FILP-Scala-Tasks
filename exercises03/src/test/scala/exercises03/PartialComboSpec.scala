package exercises03

import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class PartialComboSpec extends AnyWordSpec with ScalaCheckDrivenPropertyChecks {
  "Partial.combo" should {
    "return empty fun from empty list" in {
      forAll { i: Int =>
        assert(Partial.combo[Int, String](List.empty)(i).isEmpty)
      }
    }

    "return some fun from non empty list" in {
      val fun = Partial.combo[Int, String](List({
        case x if x == 2 => "it is 2"
      }, {
        case x if x == 3 => "it is 3"
      }, {
        case x if x == 5 => "it is 5"
      }))

      assert(fun(2).contains("it is 2"))
      assert(fun(3).contains("it is 3"))
      assert(fun(5).contains("it is 5"))
      assert(fun(42).isEmpty)
    }

    "return result in right order" in {
      val fun = Partial.combo[Int, String](List({
        case x if x == 2 => "it is first 2"
      }, {
        case x if x == 5 => "it is 5"
      }, {
        case x if x == 2 => "it is second 2"
      }))

      assert(fun(2).contains("it is first 2"))
    }
  }
}
