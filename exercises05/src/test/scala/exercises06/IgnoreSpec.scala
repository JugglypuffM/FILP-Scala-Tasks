package exercises06

import exercises06.e2_ignore.{IgnoreInstances, IgnoreSyntax}
import org.scalatest.wordspec.AnyWordSpec

class IgnoreSpec extends AnyWordSpec {
  "List, Vector, Set, Option" should {
    "have method ignore" in {
      import IgnoreInstances._
      import IgnoreSyntax._

      assert(List(1, 2, 3, 4, 5).ignore(_ > 3) == List(1, 2, 3))
      assert(List.empty[Int].ignore(_ > 3) == Nil)

      assert(Vector(1, 2, 3, 4, 5).ignore(_ > 3) == Vector(1, 2, 3))
      assert(Vector.empty[Int].ignore(_ > 3) == Nil)

      assert(Set(1, 2, 3, 4, 5).ignore(_ > 3) == Set(1, 2, 3))
      assert(Set.empty[Int].ignore(_ > 3) == Set.empty[Int])

      assert(Option(2).ignore(_ => true).isEmpty)
      assert(Option(2).ignore(_ => false).contains(2))
      assert(Option.empty[Int].ignore(_ => true).isEmpty)
    }
  }
}
