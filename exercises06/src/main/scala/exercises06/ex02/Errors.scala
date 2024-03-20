package exercises06.ex02

object Errors {
  sealed trait ParsingError

  case class InvalidPhone(str: String)         extends ParsingError
  case class InvalidAddressBookId(str: String) extends ParsingError
  case class InvalidPersonId(str: String)      extends ParsingError
  case object MissingPersonName                extends ParsingError
}
