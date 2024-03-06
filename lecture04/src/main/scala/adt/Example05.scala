package adt

object Example05 extends App {
  sealed trait Tree[+A]

  case class Node[+A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

  case object Leaf extends Tree[Nothing]

  val intTree: Tree[Int] = Node(
    value = 2,
    left = Node(3, Node(1, Leaf, Leaf), Leaf),
    right = Node(4, Leaf, Leaf)
  )

  def sum(tree: Tree[Int]): Int =
    tree match {
      case Node(value, left, right) =>
        value + sum(left) + sum(right)
      case Leaf => 0
    }

  println(sum(intTree)) // 10
}
