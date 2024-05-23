package registry

import cats.effect._
import com.comcast.ip4s._
import doobie._
import org.http4s._
import org.http4s.ember.client._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import pureconfig._
import pureconfig.generic.auto._
import registry.adapter.jdbc.sqlite
import registry.adapter.{http, kafka}
import registry.api.http.SignUp
import registry.domain.Registry
import registry.domain.service.EventHandlerAlg
import Config._

object App extends IOApp.Simple {

  override def run: IO[Unit] =
    (for {
      httpClient <- EmberClientBuilder.default[IO].build
      config = ConfigSource.defaultReference.loadOrThrow[Config]
      trustworthinessAlg = http.TrustworthinessAlg.build[IO](
        httpClient,
        http.TrustworthinessAlg.Auth(config.trustworthiness.auth.user, config.trustworthiness.auth.password),
        config.trustworthiness.uri
      )
      eventObserver = kafka.EventObserverAlg
        .build[IO](config.kafka.host, config.kafka.port, config.kafka.topic, config.kafka.group)
      transactor = Transactor.fromDriverManager[IO](
        classOf[org.sqlite.JDBC].getName,
        config.sqlite.uri,
        config.sqlite.user,
        config.sqlite.password,
        None
      )
      userAlg <- sqlite.UserAlg.build[IO](transactor).toResource
      appAlg  <- sqlite.ApplicationAlg.build[IO](transactor).toResource
      eventHandler = EventHandlerAlg.build[IO](userAlg, appAlg)
      _ <- eventObserver.start(eventHandler).toResource
      registry = Registry.build[IO](userAlg, appAlg, trustworthinessAlg)
      checkIn  = new SignUp[IO](registry)
      _ <- EmberServerBuilder
        .default[IO]
        .withHost(config.app.host)
        .withPort(config.app.port)
        .withHttpApp(checkIn.route.orNotFound)
        .build
    } yield ()).use(_ => IO.never)
}
