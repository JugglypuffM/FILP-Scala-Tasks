package examples.typeclasses

trait Foldable[F[_]] {
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B
}

object Foldable {
  def apply[F[_]](implicit ev: Foldable[F]): Foldable[F] = implicitly
}

object FoldableSyntax {
  implicit class FoldableOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def foldLeft[B](b: B)(f: (B, A) => B)(implicit foldable: Foldable[F]): B =
      foldable.foldLeft[A, B](fa, b)(f)
  }
}

object FoldableInstances {
  implicit val foldableList: Foldable[List] = new Foldable[List] {
    override def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
  }

  implicit val foldableVector: Foldable[Vector] = new Foldable[Vector] {
    override def foldLeft[A, B](fa: Vector[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
  }

  implicit val foldableOption: Foldable[Option] = new Foldable[Option] {
    override def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
  }
}
