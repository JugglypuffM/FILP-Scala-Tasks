package exercises06.data

import exercises06.typeclasses.Semigroup

case class NonEmptyList[+A](head: A, tail: List[A] = Nil)

object NonEmptyList {
  def of[A](head: A, tail: A*): NonEmptyList[A] = NonEmptyList(head, tail.toList)

  implicit def nelSemigroup[A]: Semigroup[NonEmptyList[A]] =
    (x, y) => NonEmptyList(x.head, x.tail ::: (y.head :: y.tail))
}
