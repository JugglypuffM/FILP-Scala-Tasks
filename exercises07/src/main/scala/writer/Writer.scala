package writer

import typeclasses._

case class Writer[Log, A](log: Log, value: A) {
  def tell(nextLog: Log)(implicit semigroup: Semigroup[Log]): Writer[Log, A] = ???
}

object Writer {
  implicit def monad[Log: Monoid]: Monad[Writer[Log, *]] = ???

  def tell[Log](log: Log): Writer[Log, Unit] = ???
}
