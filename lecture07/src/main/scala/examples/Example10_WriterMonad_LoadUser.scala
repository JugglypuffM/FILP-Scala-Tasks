package examples

import examples.data.Writer
import examples.typeclasses.MonadSyntax.{MonadOps, PureOps}
import examples.typeclasses.Monoid
import examples.typeclasses.MonoidInstances._

object Example10_WriterMonad_LoadUser extends App {

  private type Logs = List[String]

  object Logs {
    def single(string: String): Logs = List(string)
  }

  final case class User(name: Option[String], surname: Option[String])

  object WriterService {

    type WithLog[A] = Writer[Logs, A]

    object WithLog {
      def log(str: String): WithLog[Unit] = Writer.tell(Logs.single(str))
    }

    def loadUserName(id: Long): WithLog[Option[String]] =
      id match {
        case 1 => Option("Alice").pure[WithLog]
        case 2 => Option("Bob").pure[WithLog]
        case _ => WithLog.log(s"Can't find name by id=$id").map(_ => None)
      }

    def loadUserSurname(id: Long): WithLog[Option[String]] =
      id match {
        case 1 => Option("Smith").pure[WithLog].tell(Logs.single("Surname is Smith"))
        case 2 => Option("Johnson").pure[WithLog].tell(Logs.single("Surname is Johnson"))
        case _ => WithLog.log(s"Can't find surname by id=$id").map(_ => None)
      }

    def loadUser(id: Long): WithLog[User] =
      for {
        _       <- WithLog.log(s"Loading user name by id=$id")
        name    <- loadUserName(id)
        _       <- WithLog.log(s"Loading user surname by id=$id")
        surname <- loadUserSurname(id)
      } yield User(name, surname)
  }

  // Результат содержит:
  // 1. значение, возвращаемое методом loadUser
  // 2. логи, сделанные в процессе вычислений
  //val  (logs, user) = WriterService.loadUser(id = 1).run

  printResult(WriterService.loadUser(id = 1).run)
  printResult(WriterService.loadUser(id = 2).run)
  printResult(WriterService.loadUser(id = 3).run)

  def printResult(result: (Logs, User)): Unit = {
    val (logs, user) = result
    println(user)
    println(logs.mkString("\n"))
    println()
  }
}
