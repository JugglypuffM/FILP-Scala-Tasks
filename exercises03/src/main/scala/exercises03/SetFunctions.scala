package exercises03

object SetFunctions {
  type Set[A] = A => Boolean

  def empty: Set[Nothing] = ???

  def contains[A](s: Set[A], elem: A): Boolean = ???

  def singletonSet[A](elem: A): Set[A] = ???

  def union[A](s: Set[A], t: Set[A]): Set[A] = ???

  def intersect[A](s: Set[A], t: Set[A]): Set[A] = ???

  def diff[A](s: Set[A], t: Set[A]): Set[A] = ???

  def symmetricDiff[A](s: Set[A], t: Set[A]): Set[A] = ???

  def filter[A](s: Set[A], p: A => Boolean): Set[A] = ???

  def cartesianProduct[A, B](as: Set[A], bs: Set[B]): Set[(A, B)] = ???
}
