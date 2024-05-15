package registry.adapter.http

import cats.effect._
import io.circe.{Decoder, HCursor}
import io.circe.generic.auto._
import org.http4s._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.headers._
import registry.domain.model.{Application, User}
import registry.domain.service.TrustworthinessAlg

object TrustworthinessAlg {
  def build[F[_]: Concurrent](client: Client[F], auth: Auth, uri: Uri): TrustworthinessAlg[F] =
    new TrustworthinessAlgInterpreter[F](client, auth, uri)

  private class TrustworthinessAlgInterpreter[F[_]: Concurrent](
      client: Client[F],
      auth: Auth,
      uri: Uri
  ) extends TrustworthinessAlg[F] {

    private implicit val userEncoder: EntityEncoder[F, User] = jsonEncoderOf[F, User]

    private implicit val applicationIdDecoder: EntityDecoder[F, Application.Id] = {

      implicit val decoder: Decoder[Application.Id] = (c: HCursor) =>
        for {
          field <- c.downField("requestId").as[String]
        } yield Application.Id(field)

      jsonOf[F, Application.Id]

    }

    override def check(user: User): F[Application.Id] = {
      val request = Request[F](
        method = Method.POST,
        uri = uri.addSegment("check"),
        headers = Headers(
          Authorization(BasicCredentials(auth.username, auth.password)),
          Accept(MediaType.application.json)
        )
      ).withEntity(user)
      client.expect[Application.Id](request)
    }
  }

  case class Auth(username: String, password: String)
}
