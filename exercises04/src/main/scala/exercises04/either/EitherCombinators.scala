package exercises04.either

object EitherCombinators {

  // Необходимо самостоятельно написать методы map и flatMap
  sealed trait Either[+A, +B] {
    def orElse[EE >: A, C >: B](other: => Either[EE, C]): Either[EE, C] = (this, other) match {
      case (Right(_), _)       => this
      case (Left(_), Left(_))  => this
      case (Left(_), Right(_)) => other
    }

    def map2[AA >: A, BB, C](other: => Either[AA, BB])(f: (B, BB) => C): Either[AA, C] = (this, other) match {
      case (Right(v1), Right(v2)) => Right(f(v1, v2))
      case (Left(v), Right(_))    => Left(v)
      case (Right(_), Left(v))    => Left(v)
      case (Left(v), Left(_))     => Left(v)
    }

    def map[B1](f: B => B1): Either[A, B1] = this match {
      case Left(v)  => Left(v)
      case Right(v) => Right(f(v))
    }

    def flatMap[A1 >: A, B1](f: B => Either[A1, B1]): Either[A1, B1] = this match {
      case Left(v)  => Left(v)
      case Right(v) => f(v)
    }
  }

  case class Left[+A, +B](get: A) extends Either[A, B]

  case class Right[+A, +B](get: B) extends Either[A, B]

  object Either {
    def fromOption[A, B](option: Option[B])(a: => A): Either[A, B] = option match {
      case Some(v) => Right(v)
      case _       => Left(a)
    }

    def traverse[E, A, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] = sequence(list.map(f(_)))

    def sequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] =
      list.foldRight(Right(Nil): Either[E, List[A]])((next, prev) => next.flatMap(x => prev.map(b => x :: b)))
  }
}
