package exercises06.ex02

import exercises06.data.{NonEmptyList, Validated}
import exercises06.ex02.Domain._
import exercises06.ex02.Errors.ParsingError

object Run extends App {
  import TransformerSyntax._
  import Exercise02._

  type V[A] = Validated[NonEmptyList[ParsingError], A]

  val goodBook: RawAddressBook =
    RawAddressBook(
      "1",
      List(RawPerson("2", Some("ilya"), "+79995556677"), RawPerson("3", Some("vasya"), "+79994441122"))
    )

  val badBook: RawAddressBook =
    RawAddressBook(
      "NaN",
      List(RawPerson("kek", None, "lol"), RawPerson("cheburek", None, "123"))
    )

  println(goodBook.transformF[V, AddressBook]) // Valid(AddressBook(1,List(Person(2,ilya,Phone(+79995556677)), Person(3,vasya,Phone(+79994441122)))))
  println(badBook.transformF[V, AddressBook])  // Invalid(NonEmptyList(InvalidAddressBookId(NaN),List(InvalidPersonId(kek), MissingPersonName, InvalidPhone(lol), InvalidPersonId(cheburek), MissingPersonName, InvalidPhone(123))))
}
