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
      case Nil              => base
    }

  def sum(list: MyList[Int]): Int = foldLeft(list)(0)(_ + _)

  def reverse[A](list: MyList[A]): MyList[A] = foldLeft(list)(Nil: MyList[A])((prev, next) => Cons(next, prev))

  def last[A](myList: MyList[A]): Option[A] = foldLeft(myList)(None: Option[A])((prev, next) => Some(next))

  def size[A](myList: MyList[A]): Int = foldLeft(myList)(0)((prev, next) => prev + 1)

  def max[A](myList: MyList[A], isBigger: (A, A) => Boolean): Option[A] =
    foldLeft(myList)(None: Option[A])((prev, next) =>
      prev match {
        case Some(v) => if (isBigger(v, next)) Some(v) else Some(next)
        case None    => Some(next)
      }
    )

  def filter[A](myList: MyList[A], predicate: A => Filter.Filter): MyList[A] =
    reverse(
      foldLeft(myList)(Nil: MyList[A])((prev, next) =>
        predicate(next) match {
          case Preserve => Cons(next, prev)
          case Skip     => prev
        }
      )
    )

  object Filter {
    sealed trait Filter
    case object Skip     extends Filter
    case object Preserve extends Filter
  }

}
