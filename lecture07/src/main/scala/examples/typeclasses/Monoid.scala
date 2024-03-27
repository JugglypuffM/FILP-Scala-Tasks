package examples.typeclasses

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A: Monoid]: Monoid[A] = implicitly
}

object MonoidSyntax {
  implicit class MonoidOps[A](private val a: A) extends AnyVal {
    def |+|(b: A)(implicit s: Monoid[A]): A = s.combine(a, b)
  }
}

object MonoidInstances {
  implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
    override def combine(x: Int, y: Int): Int = x + y
    override def empty: Int                   = 0
  }

  implicit val monoidString: Monoid[String] = new Monoid[String] {
    override def combine(x: String, y: String): String = x + y
    override def empty: String                         = ""
  }

  implicit def monoidList[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def combine(x: List[A], y: List[A]): List[A] = x ::: y
    override def empty: List[A]                           = Nil
  }

  implicit def monoidOption[A: Monoid]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
      case (Some(a), Some(b)) => Some(Monoid[A].combine(a, b))
      case (None, Some(b))    => Some(Monoid[A].combine(Monoid[A].empty, b))
      case (Some(a), None)    => Some(Monoid[A].combine(a, Monoid[A].empty))
      case (None, None)       => Some(Monoid[A].empty)
    }
    override def empty: Option[A] = None
  }
}
