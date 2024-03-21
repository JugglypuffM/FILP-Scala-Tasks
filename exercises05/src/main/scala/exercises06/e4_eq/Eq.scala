package exercises06.e4_eq

trait Eq[A] {
  def eqv(a: A, b: A): Boolean
}

object EqInstances {
  implicit def basicEq[A]: Eq[A] = (x: A, y: A) => x == y
  implicit def listEq[A](implicit ev: Eq[A]): Eq[List[A]] =
    (a: List[A], b: List[A]) => a.corresponds(b)(ev.eqv)
  implicit def optionEq[A](implicit ev: Eq[A]): Eq[Option[A]] =
    (a: Option[A], b: Option[A]) => a.corresponds(b)(ev.eqv)
}

object EqSyntax {
  implicit class EqOps[A](val x: A) {
    def eqv(y: A)(implicit ev: Eq[A]): Boolean = ev.eqv(x, y)
    def ===(y: A)(implicit ev: Eq[A]): Boolean = ev.eqv(x, y)
    def !==(y: A)(implicit ev: Eq[A]): Boolean = !ev.eqv(x, y)
  }
}

object Examples {
  import EqInstances._
  import EqSyntax._

  1 eqv 1 // возвращает true
  1 === 2 // возвращает false
  1 !== 2 // возвращает true
  // 1 === "some-string" // не компилируется
  // 1 !== Some(2) // не компилируется
  List(true) === List(true) // возвращает true
}
