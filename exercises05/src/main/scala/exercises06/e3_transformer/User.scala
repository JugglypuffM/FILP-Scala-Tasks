package exercises06.e3_transformer

case class UserName(firstName: String, secondName: String, thirdName: Option[String])
case class User(id: Long, userName: UserName)
