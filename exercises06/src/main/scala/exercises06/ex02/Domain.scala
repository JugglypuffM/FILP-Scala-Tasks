package exercises06.ex02

import scala.util.matching.Regex

object Domain {
  case class RawAddressBook(id: String, persons: List[RawPerson])

  case class RawPerson(id: String, name: Option[String], phone: String)

  case class AddressBook(id: Long, persons: List[Person])

  case class Person(id: Long, name: String, phone: Phone)

  case class Phone(value: String) extends AnyVal

  object Phone {
    private val Regexp: Regex = "^(\\+7[0-9]{10})$".r

    def parse(str: String): Option[Phone] =
      str match {
        case Regexp(phone) => Some(Phone(phone))
        case _             => None
      }
  }
}
