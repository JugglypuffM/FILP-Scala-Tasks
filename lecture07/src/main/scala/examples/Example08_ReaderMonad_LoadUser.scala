package examples

import examples.data.Reader
import examples.typeclasses.MonadInstances._
import examples.typeclasses.MonadSyntax._

import scala.util.Try

/*
                               ┌────────────────────┐
┌───────────────────┐          │                    │
│      Configs      │─────────>│     Application    │
└───────────────────┘          │                    │
                               └────────────────────┘
 */

object Example08_ReaderMonad_LoadUser extends App {

  final case class HttpClientConfig(url: String, authToken: String)
  final case class DbConfig(url: String, user: String, password: String)
  final case class AppConfig(
      httpClientConfig: HttpClientConfig,
      dbConfig: DbConfig
  )

  final case class User(name: Option[String], surname: Option[String])

  object ClassicService {

    private def httpRequest(url: String, authToken: String): Try[Option[String]] = {
      println(s"Send HTTP request $url token $authToken")
      Option("result").pure[Try]
    }

    private def dbQuery(endpoint: String, user: String, password: String): Try[Option[String]] = {
      println(s"Database query $endpoint user=$user password=$password")
      Option("result").pure[Try]
    }

    private def loadUserName(id: Long)(config: HttpClientConfig): Try[Option[String]] =
      for {
        name <- httpRequest(s"${config.url}/name/$id", config.authToken)
      } yield name

    private def loadUserSurname(id: Long)(config: DbConfig): Try[Option[String]] =
      for {
        surname <- dbQuery(config.url, config.user, config.password)
      } yield surname

    def loadUser(id: Long)(config: AppConfig): Try[User] =
      for {
        name    <- loadUserName(id)(config.httpClientConfig)
        surname <- loadUserSurname(id)(config.dbConfig)
      } yield User(name, surname)

    def start: Unit = {

      val config = AppConfig(
        HttpClientConfig("http://external-service.net", "token"),
        DbConfig("postgres", "user", "password")
      )

      println(ClassicService.loadUser(id = 1)(config))
      println()

      println(ClassicService.loadUser(id = 2)(config))
      println()

      println(ClassicService.loadUser(id = 3)(config))
    }
  }

  // =========================================

  object ReaderService {

    type WithConfig[A] = Reader[AppConfig, A]

    object WithConfig {
      def getConfig[A](select: AppConfig => A): WithConfig[A] = Reader.ask[AppConfig, A](select)
    }

    private def httpRequest(url: String, authToken: String): WithConfig[Option[String]] = {
      println(s"Send HTTP request $url token $authToken")
      Option("result").pure[WithConfig]
    }

    private def dbQuery(endpoint: String, user: String, password: String): WithConfig[Option[String]] = {
      println(s"Database query $endpoint user=$user password=$password")
      Option("result").pure[WithConfig]
    }

    private def loadUserName(id: Long): WithConfig[Option[String]] =
      for {
        config <- WithConfig.getConfig(_.httpClientConfig)
        name   <- httpRequest(s"${config.url}/name/$id", config.authToken)
      } yield name

    private def loadUserSurname(id: Long): WithConfig[Option[String]] =
      for {
        config  <- WithConfig.getConfig(_.dbConfig)
        surname <- dbQuery(config.url, config.user, config.password)
      } yield surname

    def loadUser(id: Long): WithConfig[User] =
      for {
        name    <- loadUserName(id)
        surname <- loadUserSurname(id)
      } yield User(name, surname)

    def start: Unit = {

      val config = AppConfig(
        HttpClientConfig("http://external-service.net", "token"),
        DbConfig("postgres", "user", "password")
      )

      println(ReaderService.loadUser(id = 1).run(config))
      println()

      println(ReaderService.loadUser(id = 2).run(config))
      println()

      println(ReaderService.loadUser(id = 3).run(config))
    }
  }

  // Run

  ClassicService.start

  //ReaderService.start
}
