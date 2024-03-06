package exercises04.parser

case class RawUser(
    id: String,
    banned: String,
    passport: String,
    firstName: Option[String],
    secondName: Option[String],
    thirdName: Option[String]
)
