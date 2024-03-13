package exercises06

import exercises06.e4_eq.{Eq, EqInstances}
import org.scalatest.wordspec.AnyWordSpec

class EqSpec extends AnyWordSpec {
  "Eq" should {
    "compare numbers" in {
      import EqInstances._
      val eq = implicitly[Eq[Int]]
      assert(eq.eqv(1, 1) && eq.eqv(42, 42))
      assert(!eq.eqv(1, 42) && !eq.eqv(42, 1))
    }

    "compare booleans" in {
      import EqInstances._
      val eq = implicitly[Eq[Boolean]]
      assert(eq.eqv(true, true) && eq.eqv(false, false))
      assert(!eq.eqv(true, false) && !eq.eqv(false, true))
    }

    "compare options" in {
      import EqInstances._
      val intOptionEq  = implicitly[Eq[Option[Int]]]
      val boolOptionEq = implicitly[Eq[Option[Boolean]]]

      assert(intOptionEq.eqv(None, None))
      assert(boolOptionEq.eqv(None, None))

      assert(intOptionEq.eqv(Some(1), Some(1)) && intOptionEq.eqv(Some(42), Some(42)))
      assert(boolOptionEq.eqv(Some(true), Some(true)) && boolOptionEq.eqv(Some(false), Some(false)))

      assert(!intOptionEq.eqv(Some(1), Some(42)) && !intOptionEq.eqv(Some(42), Some(1)))
      assert(!boolOptionEq.eqv(Some(true), Some(false)) && !boolOptionEq.eqv(Some(false), Some(true)))
    }

    "compare lists" in {
      import EqInstances._
      val intListEq  = implicitly[Eq[List[Int]]]
      val boolListEq = implicitly[Eq[List[Boolean]]]

      assert(intListEq.eqv(Nil, Nil))
      assert(intListEq.eqv(List(42), List(42)))
      assert(intListEq.eqv(List(1, 2, 3), List(1, 2, 3)))
      assert(!intListEq.eqv(List(1, 2, 3), List(3, 1, 2)))
      assert(!intListEq.eqv(List(1, 2, 3), List(1, 2, 3, 4, 5)))
      assert(!intListEq.eqv(List(1, 2, 3, 4, 5), List(1, 2, 3)))

      assert(boolListEq.eqv(Nil, Nil))
      assert(boolListEq.eqv(List(true), List(true)))
      assert(boolListEq.eqv(List(false), List(false)))
      assert(boolListEq.eqv(List(true, true, false), List(true, true, false)))
      assert(!boolListEq.eqv(List(false, true, false), List(true, true, false)))
      assert(!boolListEq.eqv(List(false, true, false), List(true, false, false)))
      assert(!boolListEq.eqv(List(true, false, true, true, true), List(true, false, true)))
      assert(!boolListEq.eqv(List(true, false, true), List(true, false, true, true, true)))
    }

    "compare list and options with custom eq" in {
      import EqInstances._

      val intAbsEq: Eq[Int] = (x, y) => math.abs(x) == math.abs(y)

      val listIntEq: Eq[List[Int]] = {
        implicit val intEq: Eq[Int] = intAbsEq
        implicitly
      }

      val optionIntEq: Eq[Option[Int]] = {
        implicit val intEq: Eq[Int] = intAbsEq
        implicitly
      }

      assert(listIntEq.eqv(List(1, 2, 3), List(1, 2, -3)))
      assert(optionIntEq.eqv(Some(-1), Some(1)))
    }
  }
}
