package examples.data

import examples.typeclasses.Monad

final case class State[S, V](run: S => (V, S))

object State {
  // Позволяет извлечь текущее состояние
  def ask[S, StateCtx](f: S => StateCtx): State[S, StateCtx] =
    State(current => (f(current), current))

  // Позволяет обновить состояние
  def update[S](f: S => S): State[S, Unit] =
    State(current => ((), f(current)))

  implicit def monad[S]: Monad[State[S, *]] = new Monad[State[S, *]] {
    def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
      State(current =>
        fa.run(current) match {
          case (value, next) => f(value).run(next)
        }
      )

    def pure[Value](value: Value): State[S, Value] =
      State(state => (value, state))
  }
}
