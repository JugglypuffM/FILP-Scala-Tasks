package examples.data

import examples.typeclasses.Monad

final case class Reader[Ctx, Out](run: Ctx => Out)

object Reader {
  // Позволяет извлечь информацию из контекста
  def ask[Ctx, CtxInfo](f: Ctx => CtxInfo): Reader[Ctx, CtxInfo] = Reader(f)

  implicit def monad[Ctx]: Monad[Reader[Ctx, *]] = new Monad[Reader[Ctx, *]] {

    // Позволяет комбинировать Reader и прокидывать контекст по цепочке вычислений
    def flatMap[A, B](fa: Reader[Ctx, A])(f: A => Reader[Ctx, B]): Reader[Ctx, B] =
      Reader(ctx => f(fa.run(ctx)).run(ctx))

    def pure[A](a: A): Reader[Ctx, A] = Reader(_ => a)
  }
}
