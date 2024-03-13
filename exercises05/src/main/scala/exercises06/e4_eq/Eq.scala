package exercises06.e4_eq

trait Eq[A] {
  def eqv(a: A, b: A): Boolean
}

object Eq {}

object EqInstances {}

object EqSyntax {}

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
