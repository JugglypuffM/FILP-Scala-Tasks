package typeclasses

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

object Semigroup {
  @inline
  def apply[A](implicit inst: Semigroup[A]): Semigroup[A] =
    inst
}
