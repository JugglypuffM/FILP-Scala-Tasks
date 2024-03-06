package exercises04.parser

case class UserName(firstName: String, secondName: String, thirdName: Option[String])
case class Passport(series: Long, number: Long)
case class User(id: Long, userName: UserName, passport: Passport)
