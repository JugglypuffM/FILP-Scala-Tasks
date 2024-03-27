package reader

import typeclasses.Monad

case class Reader[R, A](run: R => A)

object Reader {
  implicit def monad[R]: Monad[Reader[R, *]] = ???

  def ask[R]: Reader[R, R] =
    Reader(identity)

  def ask[R, A](f: R => A): Reader[R, A] =
    Reader(f)

  def sequence[R, A](list: List[Reader[R, A]]): Reader[R, List[A]] = Reader { r =>
    list.map(_.run(r))
  }
}
