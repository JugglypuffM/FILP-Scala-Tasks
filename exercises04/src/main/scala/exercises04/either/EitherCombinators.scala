package exercises04.either

object EitherCombinators {

  // Необходимо самостоятельно написать методы map и flatMap
  sealed trait Either[+A, +B] {
    def orElse[EE >: A, C >: B](other: => Either[EE, C]): Either[EE, C] = ???

    def map2[AA >: A, BB, C](other: => Either[AA, BB])(f: (B, BB) => C): Either[AA, C] = ???
  }

  case class Left[+A, +B](get: A) extends Either[A, B]

  case class Right[+A, +B](get: B) extends Either[A, B]

  object Either {
    def fromOption[A, B](option: Option[B])(a: => A): Either[A, B] = ???

    def traverse[E, A, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

    def sequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] = ???
  }

}
