package exercises04

import org.scalatest.wordspec.AnyWordSpec

class TreeSpec extends AnyWordSpec {
  "Tree.fold" should {
    "fold tree" in {
      assert(Tree.fold(Branch(Leaf(36), Leaf(6)))(identity)(_ + _) == 42)
      assert(Tree.fold(Branch(Leaf(36), Leaf(6)))(_.toString)(_ + _) == "366")
      assert(Tree.fold(Leaf(42))(identity)(_ + _) == 42)
    }
  }

  "Tree.size" should {
    "calculate tree size" in {
      assert(Tree.size(Branch(Leaf(0), Leaf(0))) == 3)
      assert(Tree.size(Leaf(42)) == 1)
    }
  }

  "Tree.max" should {
    "find maximum from ints tree" in {
      assert(Tree.max(Branch(Leaf(-1), Leaf(1))) == 1)
      assert(Tree.max(Leaf(42)) == 42)
    }
  }

  "Tree.depth" should {
    "calculate tree depth" in {
      assert(Tree.depth(Branch(Leaf(-1), Leaf(1))) == 2)
      assert(Tree.depth(Leaf(42)) == 1)
    }
  }

  "Tree.map" should {
    "apply function to all elements of tree" in {
      val tree = Branch(Leaf(-1), Leaf(1))
      assert(Tree.map(tree)(identity) == tree)
      assert(Tree.map(tree)(_ * 10) == Branch(Leaf(-10), Leaf(10)))
    }
  }
}
