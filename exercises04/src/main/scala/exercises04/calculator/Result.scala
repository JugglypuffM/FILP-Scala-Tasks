package exercises04.calculator

sealed trait Result[+A]
case class Success[+A](value: A) extends Result[A]
case object DivisionByZero       extends Result[Nothing]
