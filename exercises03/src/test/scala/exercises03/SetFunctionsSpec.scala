package exercises03

import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.Set

class SetFunctionsSpec extends AnyWordSpec with ScalaCheckDrivenPropertyChecks {
  "Poly.contains" should {
    "check if set contains element" in {
      val set1 = Set(1, 2, 3)
      assert(SetFunctions.contains(set1, 1))
      assert(SetFunctions.contains(set1, 2))
      assert(SetFunctions.contains(set1, 3))
      assert(!SetFunctions.contains(set1, 42))
      assert(!SetFunctions.contains(set1, 50))
      assert(!SetFunctions.contains(set1, 100))
    }
  }

  "Poly.singletonSet" should {
    "create singletonSet" in {
      val singletonSet = SetFunctions.singletonSet(42)
      assert(singletonSet(42))
      forAll { i: Int =>
        if (i != 42)
          assert(!singletonSet(i))
      }
    }
  }

  "Poly.union" should {
    "union sets" in {
      val set1                            = Set(1, 2, 3)
      val set2                            = Set(3, 4, 5)
      val unionSet: SetFunctions.Set[Int] = SetFunctions.union(set1, set2)
      assert(set1.forall(unionSet))
      assert(set2.forall(unionSet))
    }
  }

  "Poly.intersect" should {
    "intersect sets" in {
      val set1                                = Set(1, 2, 3)
      val set2                                = Set(3, 4, 5)
      val intersectSet: SetFunctions.Set[Int] = SetFunctions.intersect(set1, set2)
      assert((set1 intersect set2).forall(intersectSet))
      assert((set1 diff set2).forall(!intersectSet(_)))
    }
  }

  "Poly.diff" should {
    "diff sets" in {
      val set1                           = Set(1, 2, 3)
      val set2                           = Set(3, 4, 5)
      val diffSet: SetFunctions.Set[Int] = SetFunctions.diff(set1, set2)
      assert((set1 diff set2).forall(diffSet))
    }
  }

  "Poly.symmetricDiff" should {
    "symmetricDiff sets" in {
      val set1 = Set(1, 2, 3, 4, 5)
      val set2 = Set(3, 4, 5, 6, 7)
      val symmetricDiffSet: SetFunctions.Set[Int] =
        SetFunctions.symmetricDiff(set1, set2)
      assert(symmetricDiffSet(1))
      assert(symmetricDiffSet(2))
      assert(!symmetricDiffSet(3))
      assert(!symmetricDiffSet(4))
      assert(!symmetricDiffSet(5))
      assert(symmetricDiffSet(6))
      assert(symmetricDiffSet(7))
      assert(!symmetricDiffSet(42))
    }
  }

  "Poly.filter" should {
    "filter set" in {
      val set                                = List.iterate(0, 10)(_ + 1).toSet
      val f                                  = (_: Int) > 4
      val filteredSet: SetFunctions.Set[Int] = SetFunctions.filter(set, f)
      assert(set.filter(f).forall(filteredSet))
      assert(set.diff(set.filter(f)).forall(!filteredSet(_)))
    }
  }

  "Poly.cartesianProduct" should {
    "cartesianProduct sets" in {
      val set1 = Set(1, 2, 3)
      val set2 = Set("a", "b")
      val cartesianProductSet: SetFunctions.Set[(Int, String)] =
        SetFunctions.cartesianProduct(set1, set2)
      assert(set1.zip(set2).forall(cartesianProductSet))
      assert(!cartesianProductSet((42, "a")))
      assert(!cartesianProductSet((42, "b")))
    }
  }
}
