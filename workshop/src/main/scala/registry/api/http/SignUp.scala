package registry.api.http

import cats.data.Validated.{Invalid, Valid}
import cats.effect.kernel.Concurrent
import cats.syntax.all._
import io.circe.generic.auto._
import org.http4s._
import org.http4s.circe.jsonOf
import org.http4s.dsl.Http4sDsl
import registry.domain.Registry
import registry.domain.model.{User, ValidationError}

class SignUp[F[_]: Concurrent](registry: Registry[F]) extends Http4sDsl[F] {

  private case class UserRaw(
      name: String,
      surname: String,
      patronymic: Option[String],
      passport: String,
      phoneNumber: String
  )

  private implicit val userDecoder: EntityDecoder[F, UserRaw] = jsonOf[F, UserRaw]

  val route: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "signUp" =>
      for {
        userRawE <- req.attemptAs[UserRaw].value
        result <- userRawE match {
          case Right(ur) =>
            User(ur.name, ur.surname, ur.patronymic, ur.passport, ur.phoneNumber) match {
              case Valid(user) =>
                registry
                  .signUp(user)
                  .flatMap(_ => Ok("Ваша заявка на регистрацию принята"))
                  .handleErrorWith(handleE)
              case Invalid(errors) =>
                //TODO: составить отформатированное сообщение об ошибках валидации
                val message: String = ???
                BadRequest(message)
            }
          case Left(err) => BadRequest(err.getMessage())
        }
      } yield result
  }

  //TODO: обработать ошибки бизнес логики
  private def handleE(err: Throwable): F[Response[F]] = ???

}
