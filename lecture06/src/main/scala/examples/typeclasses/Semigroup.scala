package examples.typeclasses

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

object Semigroup {
  def apply[A: Semigroup]: Semigroup[A] = implicitly
}

object SemigroupInstances {
  implicit val semigroupInt: Semigroup[Int] = new Semigroup[Int] {
    override def combine(x: Int, y: Int): Int = x + y
  }

  implicit val semigroupString: Semigroup[String] = new Semigroup[String] {
    override def combine(x: String, y: String): String = x + y
  }

  implicit def semigroupList[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
    override def combine(x: List[A], y: List[A]): List[A] = x ::: y
  }
}

object SemigroupSyntax {
  implicit class SemigroupOps[A](private val a: A) extends AnyVal {
    def |+|(b: A)(implicit s: Semigroup[A]): A = s.combine(a, b)
  }
}
