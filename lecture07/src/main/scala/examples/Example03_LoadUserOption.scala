package examples

object Example03_LoadUserOption extends App {

  object Service {

    case class User(id: String, name: String, surname: String)

    def loadUserOption(tag: String): Option[User] =
      for {
        id      <- idByTag(tag)
        name    <- nameById(id)
        surname <- surnameById(id)
      } yield User(id, name, surname)

    def idByTag(tag: String): Option[String] = Some(s"$tag-id")

    def nameById(id: String): Option[String] =
      id match {
        case "123-id" => Some("Alice")
        case "456-id" => Some("Bob")
        case _        => None
      }

    def surnameById(id: String): Option[String] =
      id match {
        case "123-id" => Some("Johnson")
        case "657-id" => Some("Anderson")
        case _        => None
      }
  }

  println(Service.loadUserOption("123"))
  println(Service.loadUserOption("42"))
}
