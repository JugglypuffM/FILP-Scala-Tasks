package exercises06.ex02

object TransformerSyntax {
  implicit class TransformerOps[From](private val from: From) extends AnyVal {
    def transformF[F[_], To](implicit transformerF: TransformerF[F, From, To]): F[To] =
      transformerF.transform(from)
  }
}
