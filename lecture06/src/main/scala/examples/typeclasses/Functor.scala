package examples.typeclasses

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  def apply[F[_]](implicit ev: Functor[F]): Functor[F] = implicitly
}

object FunctorSyntax {
  implicit class FunctorOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit functor: Functor[F]): F[B] =
      functor.map[A, B](fa)(f)
  }
}

object FunctorInstances {

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit val vectorFunctor: Functor[Vector] = new Functor[Vector] {
    override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)
  }
}
