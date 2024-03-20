package exercises06.typeclasses

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit inst: Monoid[A]): Monoid[A] =
    inst
}
