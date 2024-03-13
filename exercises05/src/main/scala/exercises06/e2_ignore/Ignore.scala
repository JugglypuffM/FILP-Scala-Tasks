package exercises06.e2_ignore

// отбрасывает значения, на которых предикат выдал true
trait Ignore[M[_]] {
  def ignore[A](m: M[A])(f: A => Boolean): M[A]
}

object Ignore {
  def apply[M[_]: Ignore]: Ignore[M] = ???
}

object IgnoreInstances {}

object IgnoreSyntax {
  // возможно, стоит изменить сигнатуру
  implicit class IgnoreOps[M[_], A](m: M[A]) {
    ???
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
