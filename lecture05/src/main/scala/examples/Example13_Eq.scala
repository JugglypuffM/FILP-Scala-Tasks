package examples

import scala.language.implicitConversions

// Тайпкласс
trait Eq[A] {
  def eqv(x: A, y: A): Boolean
}

// Объект-компаньон
object Eq {
  def apply[A](implicit ev: Eq[A]): Eq[A] = ev
}

// Инстансы тайпкласса для Int и String
object EqInstances {
  implicit val eqInt: Eq[Int] = new Eq[Int] {
    def eqv(x: Int, y: Int): Boolean = x == y
  }

  implicit val eqString: Eq[String] = new Eq[String] {
    def eqv(x: String, y: String): Boolean = x == y
  }
}

// Синтаксис
object EqSyntax {
  implicit class EqOps[A](private val x: A) extends AnyVal {
    def eqv(y: A)(implicit ev: Eq[A]): Boolean = ev.eqv(x, y)
    def ===(y: A)(implicit ev: Eq[A]): Boolean = ev.eqv(x, y)
    def =!=(y: A)(implicit ev: Eq[A]): Boolean = !ev.eqv(x, y)
  }
}

object Example13_Eq extends App {

  import EqInstances._
  import EqSyntax._

  Eq[Int].eqv(2 + 2, 4) // result: true

  "Hello" === "world" // result: false
  "Hello" =!= "world" // result: true

  //"Hello" === 42 // Error

  case class User(name: String, age: Int)

  object User {
    implicit val eqUser: Eq[User] = new Eq[User] {
      def eqv(x: User, y: User): Boolean = x.name === y.name && x.age === y.age
    }
  }

  val mark: User = User("Mark", 25)
  val joe: User  = User("Joe", 33)

  mark === joe // result: false
}
