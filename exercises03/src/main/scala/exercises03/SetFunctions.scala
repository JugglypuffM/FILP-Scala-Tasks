package exercises03

object SetFunctions {
  type Set[A] = A => Boolean

  def empty: Set[Nothing] = A => false

  def contains[A](s: Set[A], elem: A): Boolean = s(elem)

  def singletonSet[A](elem: A): Set[A] = a => a == elem

  def union[A](s: Set[A], t: Set[A]): Set[A] = a => s(a) || t(a)

  def intersect[A](s: Set[A], t: Set[A]): Set[A] = a => s(a) && t(a)

  def diff[A](s: Set[A], t: Set[A]): Set[A] = a => s(a) && !intersect(s, t)(a)

  def symmetricDiff[A](s: Set[A], t: Set[A]): Set[A] = a => union(s, t)(a) && !intersect(s, t)(a)

  def filter[A](s: Set[A], p: A => Boolean): Set[A] = a => p(a)

  def cartesianProduct[A, B](as: Set[A], bs: Set[B]): Set[(A, B)] = a => as(a._1) && bs(a._2)
}
