package examples

object Example04_LoadUserEither extends App {

  object Service {

    case class User(id: String, name: String, surname: String)

    type EitherThrow[A] = Either[Throwable, A]

    def loadUserEither(tag: String): EitherThrow[User] =
      for {
        id      <- idByTag(tag)
        name    <- nameById(id)
        surname <- surnameById(id)
      } yield User(id, name, surname)

    def idByTag(tag: String): EitherThrow[String] = Right(s"$tag-id")

    def nameById(id: String): EitherThrow[String] =
      id match {
        case "123-id" => Right("Alice")
        case "456-id" => Right("Bob")
        case _        => Left(new Exception("Name not found"))
      }

    def surnameById(id: String): EitherThrow[String] =
      id match {
        case "123-id" => Right("Johnson")
        case "657-id" => Right("Anderson")
        case _        => Left(new Exception("Surname not found"))
      }
  }

  println(Service.loadUserEither("123"))
  println(Service.loadUserEither("42"))
}
