package exercises06.e2_ignore

// отбрасывает значения, на которых предикат выдал true
trait Ignore[M[_]] {
  def ignore[A](m: M[A])(f: A => Boolean): M[A]
}

object Ignore {
  def apply[M[_]: Ignore]: Ignore[M] = implicitly
}

object IgnoreInstances {
  implicit val ignoreList: Ignore[List] = new Ignore[List] {
    def ignore[A](m: List[A])(f: A => Boolean): List[A] = m.filterNot(f)
  }
  implicit val ignoreOption: Ignore[Option] = new Ignore[Option] {
    def ignore[A](m: Option[A])(f: A => Boolean): Option[A] = m.filterNot(f)
  }
  implicit val ignoreVector: Ignore[Vector] = new Ignore[Vector] {
    def ignore[A](m: Vector[A])(f: A => Boolean): Vector[A] = m.filterNot(f)
  }
  implicit val ignoreSet: Ignore[Set] = new Ignore[Set] {
    def ignore[A](m: Set[A])(f: A => Boolean): Set[A] = m.filterNot(f)
  }

}

object IgnoreSyntax {
  // возможно, стоит изменить сигнатуру
  implicit class IgnoreOps[M[_]: Ignore, A](m: M[A]) {
    def ignore(f: A => Boolean): M[A] = Ignore[M].ignore(m)(f)
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
