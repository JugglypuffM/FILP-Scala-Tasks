package exercises04

import scala.annotation.tailrec

sealed trait Tree[+A]
final case class Leaf[A](value: A)                        extends Tree[A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Необходимо реализовать операции на бинарном дереве
object Tree {
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(value)         => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(value) => 1
    case _           => fold(t)(_ => 1)(_ + _) + 1
  }

  def max(t: Tree[Int]): Int = fold(t)(identity)((left, right) => if (left > right) left else right)

  def depth[A](t: Tree[A]): Int = fold(t)(_ => 1)((left, _) => left + 1)

  // тут может пригодиться явное указание типа
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value)         => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }
}
