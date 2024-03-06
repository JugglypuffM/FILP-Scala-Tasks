package exercises04

sealed trait Tree[+A]
final case class Leaf[A](value: A)                        extends Tree[A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Необходимо реализовать операции на бинарном дереве
object Tree {
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = ???

  def size[A](t: Tree[A]): Int = ???

  def max(t: Tree[Int]): Int = ???

  def depth[A](t: Tree[A]): Int = ???

  // тут может пригодиться явное указание типа
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = ???
}
