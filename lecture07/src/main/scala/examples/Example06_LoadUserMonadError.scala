package examples

import examples.typeclasses.MonadError
import examples.typeclasses.MonadError.Syntax._
import examples.typeclasses.MonadErrorInstances._
import examples.typeclasses.MonadSyntax._

import scala.util.Try

object Example06_LoadUserMonadError extends App {

  type MonadThrow[F[_]] = MonadError[F, Throwable]

  object MonadThrow {
    def apply[F[_]](implicit mt: MonadThrow[F]): MonadThrow[F] = mt
  }

  object Service {
    case class User(id: String, name: String, surname: String)

    def loadUser[F[_]: MonadThrow](tag: String): F[User] =
      for {
        id      <- idByTag[F](tag)
        name    <- nameById[F](id)
        surname <- surnameById[F](id)
      } yield User(id, name, surname)

    def idByTag[F[_]: MonadThrow](tag: String): F[String] =
      s"$tag-id".pure[F]

    def nameById[F[_]: MonadThrow](id: String): F[String] =
      id match {
        case "123-id" => "Alice".pure[F]
        case "456-id" => "Bob".pure[F]
        case _        => MonadThrow[F].raiseError(new RuntimeException("Name not found"))
      }

    def surnameById[F[_]: MonadThrow](id: String): F[String] =
      id match {
        case "123-id" => "Johnson".pure[F]
        case "657-id" => "Anderson".pure[F]
        case _        => MonadThrow[F].raiseError(new RuntimeException("Surname not found"))
      }
  }

  println(Service.loadUser[Either[Throwable, *]]("123"))
  println(Service.loadUser[Either[Throwable, *]]("456"))
  println(Service.loadUser[Either[Throwable, *]]("657"))

  println()

  println(Service.loadUser[Try]("123"))
  println(Service.loadUser[Try]("456"))
  println(Service.loadUser[Try]("657"))

  // Пример обработки ошибки
  Service.loadUser[Try]("657").handleErrorWith[Throwable] { error =>
    println(s"Loading user failed with error: $error")
      .pure[Try]
      .flatMap(_ => MonadThrow[Try].raiseError(new Exception("Boom!")))
  }
}
