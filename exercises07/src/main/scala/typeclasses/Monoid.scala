package typeclasses

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit inst: Monoid[A]): Monoid[A] =
    inst

  object syntax {
    implicit class MonoidOps[A](private val x: A) extends AnyVal {
      def |+|(y: A)(implicit monoid: Monoid[A]): A =
        monoid.combine(x, y)
    }
  }
}
