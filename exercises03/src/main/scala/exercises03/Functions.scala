package exercises03

class Functions {
  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def andThen[A, B, C](f: A => B)(g: B => C): A => C = a => g(f(a))

  def compose[A, B, C](f: B => C)(g: A => B): A => C = a => f(g(a))

  def const[A, B](b: B): A => B = a => b

  def liftOption[A, B](f: A => B): A => Option[B] = a => Option(f(a))

  def chain[A](functions: List[A => A]): A => Option[A] =
    a =>
      functions match {
        case List() => None
        case _      => functions.foldLeft(Option(a))((res, func) => res.flatMap(value => Some(func(value))))
      }

  def zip[A, B, C](f: A => B, g: A => C): A => (B, C) = a => (f(a), g(a))

  def unzip[A, B, C](f: A => (B, C)): (A => B, A => C) = (a => f(a)._1, a => f(a)._2)

}
