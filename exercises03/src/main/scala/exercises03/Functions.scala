package exercises03

class Functions {
  def curry[A, B, C](f: (A, B) => C): A => B => C = ???

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = ???

  def andThen[A, B, C](f: A => B)(g: B => C): A => C = ???

  def compose[A, B, C](f: B => C)(g: A => B): A => C = ???

  def const[A, B](b: B): A => B = ???

  def liftOption[A, B](f: A => B): A => Option[B] = ???

  def chain[A](functions: List[A => A]): A => Option[A] = ???

  def zip[A, B, C](f: A => B, g: A => C): A => (B, C) = ???

  def unzip[A, B, C](f: A => (B, C)): (A => B, A => C) = ???

}
