package exercises03

class Functions {
  def curry[A, B, C](f: (A, B) => C): A => B => C = A => B => f(A, B)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (A, B) => f(A)(B)

  def andThen[A, B, C](f: A => B)(g: B => C): A => C = A => g(f(A))

  def compose[A, B, C](f: B => C)(g: A => B): A => C = A => f(g(A))

  def const[A, B](b: B): A => B = A => b

  def liftOption[A, B](f: A => B): A => Option[B] = A => Option(f(A))

  def chain[A](functions: List[A => A]): A => Option[A] =
    A =>
      functions match {
        case List() => None
        case _      => functions.foldLeft(Option(A))((res, func) => res.flatMap(value => Some(func(value))))
      }

  def zip[A, B, C](f: A => B, g: A => C): A => (B, C) = A => (f(A), g(A))

  def unzip[A, B, C](f: A => (B, C)): (A => B, A => C) = (A => f(A)._1, A => f(A)._2)

}
