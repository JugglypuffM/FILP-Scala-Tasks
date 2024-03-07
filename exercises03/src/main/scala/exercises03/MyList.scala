package exercises03

import exercises03.MyList.Filter.{Preserve, Skip}

import scala.annotation.tailrec
import scala.runtime.Nothing$

sealed trait MyList[+A]

final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

case object Nil extends MyList[Nothing]

object MyList {
  @tailrec
  def foldLeft[A, B](list: MyList[A])(base: B)(f: (B, A) => B): B =
    list match {
      case Cons(head, tail) => foldLeft(tail)(f(base, head))(f)
      case Nil => base
    }

  def sum(list: MyList[Int]): Int = foldLeft(list)(0)(_+_)

  def reverse[A](list: MyList[A]): MyList[A] = foldLeft(list)(Nil: MyList[A])((B, A) => Cons(A, B))

  def last[A](myList: MyList[A]): Option[A] = foldLeft(myList)(None: Option[A])((B, A) => Some(A))

  def size[A](myList: MyList[A]): Int = foldLeft(myList)(0)((B, A) => B+1)

  def max[A](myList: MyList[A], isBigger: (A, A) => Boolean): Option[A] = foldLeft(myList)(None: Option[A])((A1, A2) => A1 match {
    case Some(v) => if (isBigger(v, A2)) Some(v) else Some(A2)
    case None => Some(A2)
  })

  def filter[A](myList: MyList[A], predicate: A => Filter.Filter): MyList[A] = reverse(foldLeft(myList)(Nil: MyList[A])((B, A) => predicate(A) match {
    case Preserve => Cons(A, B)
    case Skip => B
  }))

  object Filter {
    sealed trait Filter
    case object Skip     extends Filter
    case object Preserve extends Filter
  }

}
