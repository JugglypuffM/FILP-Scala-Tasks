package registry

import org.http4s._
import pureconfig._
import pureconfig.error.ExceptionThrown
import com.comcast.ip4s._

case class Config(trustworthiness: Config.Trustworthiness, kafka: Config.Kafka, sqlite: Config.Sqlite, app: Config.App)
object Config {
  case class Trustworthiness(auth: Trustworthiness.Auth, uri: Uri)
  object Trustworthiness {
    case class Auth(user: String, password: String)
  }

  case class Kafka(host: String, port: Int, topic: String, group: String)

  case class Sqlite(uri: String, user: String, password: String)

  case class App(host: Host, port: Port)

  implicit val uriReader: ConfigReader[Uri] = ConfigReader.fromString { str =>
    Uri.fromString(str) match {
      case Right(v)  => Right(v)
      case Left(err) => Left(ExceptionThrown(err))
    }
  }

  implicit val hostReader: ConfigReader[Host] = ConfigReader.fromString { str =>
    Host.fromString(str) match {
      case Some(h) => Right(h)
      case None    => Left(ExceptionThrown(new Exception("Incorrect host app")))
    }
  }

  implicit val portReader: ConfigReader[Port] = ConfigReader.fromString { str =>
    Port.fromString(str) match {
      case Some(p) => Right(p)
      case None    => Left(ExceptionThrown(new Exception("Incorrect port app")))
    }
  }
}
