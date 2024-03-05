package exercises03

import exercises03.MyList.Filter.{Preserve, Skip}

import scala.annotation.tailrec

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

  def reverse[A](list: MyList[A]): MyList[A] = {
    @tailrec
    def inner(list: MyList[A], acc: MyList[A]): MyList[A] = list match {
      case Cons(head, tail) => inner(tail, Cons(head, acc))
      case Nil              => acc
    }
    inner(list, Nil)
  }

  def last[A](myList: MyList[A]): Option[A] = {
    @tailrec
    def inner(list: MyList[A], acc: Option[A]): Option[A] = list match {
      case Cons(head, tail) => val tmp = Option(head); inner(tail, tmp)
      case Nil              => acc
    }
    inner(myList, None)
  }

  def size[A](myList: MyList[A]): Int = foldLeft(myList)(0)((B, _) => B + 1)

  def max[A](myList: MyList[A], isBigger: (A, A) => Boolean): Option[A] = {
    @tailrec
    def inner(list: MyList[A], acc: Option[A]): Option[A] = list match {
      case Cons(head, tail) =>
        val tmp = acc match {
          case Some(v) => if (isBigger(v, head)) Some(v) else Some(head)
          case None    => Some(head)
        }
        inner(tail, tmp)
      case Nil => acc
    }
    inner(myList, None)
  }

  def filter[A](myList: MyList[A], predicate: A => Filter.Filter): MyList[A] = {
    @tailrec
    def inner(list: MyList[A], acc: MyList[A]): MyList[A] = list match {
      case Cons(head, tail) =>
        predicate(head) match {
          case Preserve => inner(tail, Cons(head, acc))
          case Skip     => inner(tail, acc)
        }
      case Nil => acc
    }
    reverse(inner(myList, Nil))
  }

  object Filter {
    sealed trait Filter
    case object Skip     extends Filter
    case object Preserve extends Filter
  }

}
