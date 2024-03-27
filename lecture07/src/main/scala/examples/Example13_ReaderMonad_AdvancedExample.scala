package examples

import examples.data.Reader
import examples.typeclasses.MonadSyntax._

import scala.util.Random

/*
Пример использования Reader монады на проде.
Есть условный сервис (бекенд-приложение) со стандартной трехслойной архитектурой:
- слой данных
- сервисный слой (содержит бизнес логику)
- слой контроллеров (http-роутов)
На каждом уровне логируем сообщения и задача - идентифицировать логи конкретного запроса

  Application layers
┌────────────────────┐
│ Controller         │
│                    │
│  logger.log(...)   │
│────────────────────│
│ Service            │
│                    │
│  logger.log(...)   │
│────────────────────│
│ Data layer         │
│                    │
│  logger.log(...)   │
└────────────────────┘
 */

object Example13_ReaderMonad_AdvancedExample extends App {

  object BackendApplication {

    case class User()
    type RequestId = String

    // Тип вычисления строится из Reader монады и контексте содержит RequestId,
    // который доступен во всем приложении
    type WithContext[A] = Reader[RequestId, A]

    class RequestIdGenerator {
      def generate(): RequestId = Random.alphanumeric.take(4).mkString("").toUpperCase
    }

    class Logger {
      def log(message: String): WithContext[Unit] = Reader.ask { requestId =>
        println(s"[$requestId] $message")
      }
    }

    // Слой доступа к данным
    class UsersRepo(logger: Logger) {
      def getUsers: WithContext[List[User]] =
        for {
          users <- List(User()).pure[WithContext]
          _     <- logger.log("Get users from data layer")
        } yield users
    }

    // Сервисный слой (с бизнес логикой приложения)
    class UsersService(repo: UsersRepo, logger: Logger) {
      def getUsers: WithContext[List[User]] =
        for {
          users <- repo.getUsers
          _     <- logger.log("Get users from service layer")
        } yield users
    }

    // Слой контроллеров (http-роутов)
    class UsersController(service: UsersService, logger: Logger) {
      def handle(): WithContext[List[User]] =
        for {
          users <- service.getUsers
          _     <- logger.log("Get users from controller")
        } yield users
    }
  }

  import BackendApplication._

  val logger: Logger                         = new Logger
  val requestIdGenerator: RequestIdGenerator = new RequestIdGenerator

  val repo: UsersRepo             = new UsersRepo(logger)
  val service: UsersService       = new UsersService(repo, logger)
  val controller: UsersController = new UsersController(service, logger)

  // Представим, что клиенты делают запросы
  // Для каждого запроса генерируется уникальный requestId,
  // который используется в логгере

  // Первый запрос
  controller.handle().run(requestIdGenerator.generate())

  // Второй запрос
  controller.handle().run(requestIdGenerator.generate())

  // N-ый запрос
  controller.handle().run(requestIdGenerator.generate())
}
