package exercises06.ex02

trait TransformerF[F[_], From, To] {
  def transform(from: From): F[To]
}
