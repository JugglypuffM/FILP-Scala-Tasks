package exercises06.e2_ignore

// отбрасывает значения, на которых предикат выдал true
trait Ignore[M[_]] {
  def ignore[A](m: M[A])(f: A => Boolean): M[A]
}

object Ignore {
  def apply[M[_]: Ignore]: Ignore[M] = ???
}

object IgnoreInstances {
  implicit val ignoreList: Ignore[List] = new Ignore[List] {
    def ignore[A](m: List[A])(f: A => Boolean): List[A] = for (x <- m if !f(x)) yield x
  }
  implicit val ignoreOption: Ignore[Option] = new Ignore[Option] {
    def ignore[A](m: Option[A])(f: A => Boolean): Option[A] = m match {
      case Some(v) => if (!f(v)) Some(v) else None
      case None => None
    }
  }
  implicit val ignoreVector: Ignore[Vector] = new Ignore[Vector] {
    def ignore[A](m: Vector[A])(f: A => Boolean): Vector[A] = for (x <- m if !f(x)) yield x
  }
  implicit val ignoreSet: Ignore[Set] = new Ignore[Set] {
    def ignore[A](m: Set[A])(f: A => Boolean): Set[A] = for (x <- m if !f(x)) yield x
  }

}

object IgnoreSyntax {
  // возможно, стоит изменить сигнатуру
  implicit class IgnoreOps[M[_], A](val m: M[A]) extends AnyVal {
    def ignore(f: A => Boolean)(implicit io: Ignore[M]): M[A] = io.ignore(m)(f)
  }
}

object Examples {
  import IgnoreInstances._
  import IgnoreSyntax._

  val list: List[Int]     = List[Int](1, 2, 3, 4, 5).ignore(_ => true)
  val some: Option[Int]   = Option(2).ignore(_ => true)
  val none: Option[Int]   = Option.empty[Int].ignore(_ => true)
  val vector: Vector[Int] = Vector[Int]().ignore(_ => true)
  val set: Set[Int]       = Set[Int]().ignore(_ => true)
}
