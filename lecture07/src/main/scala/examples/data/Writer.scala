package examples.data

import examples.typeclasses.{Monad, Monoid}
import examples.typeclasses.MonoidSyntax.MonoidOps

final case class Writer[Log, Value](run: (Log, Value)) extends AnyVal {
  // Позволяет записать данные в лог
  def tell(message: Log)(implicit monoid: Monoid[Log]): Writer[Log, Value] = run match {
    case (log, value) => Writer((log |+| message, value))
  }
}

object Writer {

  // Позволяет записать данные в лог
  def tell[Log: Monoid](log: Log): Writer[Log, Unit] = Writer((log, ()))

  implicit def monad[Log: Monoid]: Monad[Writer[Log, *]] = new Monad[Writer[Log, *]] {
    def flatMap[A, B](fa: Writer[Log, A])(f: A => Writer[Log, B]): Writer[Log, B] =
      fa.run match {
        case (log, value) =>
          f(value).run match {
            case (nextLog, nextValue) => Writer((log |+| nextLog, nextValue))
          }
      }

    def pure[A](a: A): Writer[Log, A] = Writer((Monoid[Log].empty, a))
  }
}
