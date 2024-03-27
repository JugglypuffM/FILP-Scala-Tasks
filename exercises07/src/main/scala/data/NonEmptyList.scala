package data

case class NonEmptyList[A](head: A, tail: List[A] = Nil) {
  def reduce[B](f: (A, A) => A): A =
    tail.foldLeft(head)(f)

  def map[B](f: A => B): NonEmptyList[B] = NonEmptyList(f(head), tail.map(f))
}

object NonEmptyList {
  def of[A](head: A, tail: A*): NonEmptyList[A] = NonEmptyList(head, tail.toList)
}
