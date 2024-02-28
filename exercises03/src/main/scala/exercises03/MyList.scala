package exercises03

sealed trait MyList[+A]

final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

case object Nil extends MyList[Nothing]

object MyList {
  def foldLeft[A, B](list: MyList[A])(base: B)(f: (B, A) => B): B = ???

  def sum(list: MyList[Int]): Int = ???

  def reverse[A](list: MyList[A]): MyList[A] = ???

  def last[A](myList: MyList[A]): Option[A] = ???

  def size[A](myList: MyList[A]): Int = ???

  def max[A](myList: MyList[A], isBigger: (A, A) => Boolean): Option[A] = ???

  def filter[A](myList: MyList[A], predicate: A => Filter.Filter): MyList[A] = ???

  object Filter {
    sealed trait Filter
    case object Skip     extends Filter
    case object Preserve extends Filter
  }

}
