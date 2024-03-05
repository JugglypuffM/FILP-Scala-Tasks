package exercises03

object SetFunctions {
  type Set[A] = A => Boolean

  def empty: Set[Nothing] = A => false

  def contains[A](s: Set[A], elem: A): Boolean = s(elem)

  def singletonSet[A](elem: A): Set[A] = A => A == elem

  def union[A](s: Set[A], t: Set[A]): Set[A] = A => s(A) || t(A)

  def intersect[A](s: Set[A], t: Set[A]): Set[A] = A => s(A) && t(A)

  def diff[A](s: Set[A], t: Set[A]): Set[A] = A => s(A) && !intersect(s, t)(A)

  def symmetricDiff[A](s: Set[A], t: Set[A]): Set[A] = A => union(s, t)(A) && !intersect(s, t)(A)

  def filter[A](s: Set[A], p: A => Boolean): Set[A] = A => p(A)

  def cartesianProduct[A, B](as: Set[A], bs: Set[B]): Set[(A, B)] = A => as(A._1) && bs(A._2)
}
