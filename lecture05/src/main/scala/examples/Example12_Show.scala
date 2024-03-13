package examples

import scala.language.implicitConversions

// Тайпкласс
trait Show[A] {
  def show(value: A): String
}

// Объект-компаньон
object Show {
  def apply[A](implicit ev: Show[A]): Show[A] = ev
}

// Инстансы тайпкласса для Int и String
object ShowInstances {
  implicit val showInt: Show[Int] = new Show[Int] {
    def show(value: Int): String = value.toString
  }

  implicit val showString: Show[String] = new Show[String] {
    def show(value: String): String = value
  }
}

// Синтаксис
object ShowSyntax {
  implicit class ShowOps[A](private val value: A) extends AnyVal {
    def show(implicit ev: Show[A]): Unit = ev.show(value)
  }
}

object Example12_Show extends App {

  import ShowInstances._
  import ShowSyntax._

  val meaningOfLife: Int = 42

  Show[Int].show(meaningOfLife) // result: "42"
  // or
  meaningOfLife.show // result: "42"

  case class User(name: String, age: Int)

  object User {
    implicit val showUser: Show[User] = new Show[User] {
      def show(user: User): String = s"User(name = ${user.name}, age = ${user.age})"
    }
  }

  val user: User = User("Mark", 25)
  user.show
  // result: "User(name = Mark, age = 25)"
}
