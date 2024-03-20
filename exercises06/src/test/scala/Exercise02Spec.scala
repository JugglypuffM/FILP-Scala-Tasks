import exercises06.data._
import exercises06.data.Validated._
import exercises06.ex02.Domain._
import exercises06.ex02.Errors._
import exercises06.ex02.TransformerSyntax._
import exercises06.ex02.Exercise02._
import org.scalatest.Assertion
import org.scalatest.wordspec.AnyWordSpec

class Exercise02Spec extends AnyWordSpec {
  type V[A] = Validated[NonEmptyList[ParsingError], A]

  "Exercise02" should {
    "goodBook" in {
      assert(
        RawAddressBook(
          "1",
          List(RawPerson("2", Some("ilya"), "+79995556677"), RawPerson("3", Some("vasya"), "+79994441122"))
        ).transformF[V, AddressBook] == Valid(
          AddressBook(1, List(Person(2, "ilya", Phone("+79995556677")), Person(3, "vasya", Phone("+79994441122"))))
        )
      )
    }

    def toList[A](nel: NonEmptyList[A]): List[A] = nel.head :: nel.tail

    "badBook" in {
      def unorderedEqual[A](list1: List[A], list2: List[A]): Boolean =
        list1.forall(list2.contains) && list2.forall(list1.contains)

      def check(book: RawAddressBook, expectedErrors: List[ParsingError]): Assertion =
        assert(book.transformF[V, AddressBook] match {
          case Valid(_)   => false
          case Invalid(e) => unorderedEqual(toList(e), expectedErrors)
        })

      val allErrors = List(
        InvalidAddressBookId("NaN"),
        InvalidPersonId("kek"),
        MissingPersonName,
        InvalidPhone("lol"),
        InvalidPersonId("cheburek"),
        MissingPersonName,
        InvalidPhone("123")
      )
      check(RawAddressBook("NaN", List(RawPerson("kek", None, "lol"), RawPerson("cheburek", None, "123"))), allErrors)
      check(
        RawAddressBook("123", List(RawPerson("kek", None, "lol"), RawPerson("cheburek", None, "123"))),
        allErrors.drop(1)
      )
      check(
        RawAddressBook("123", List(RawPerson("456", None, "lol"), RawPerson("cheburek", None, "123"))),
        allErrors.drop(2)
      )
      check(
        RawAddressBook("123", List(RawPerson("456", Some("kek"), "lol"), RawPerson("cheburek", None, "123"))),
        allErrors.drop(3)
      )
      check(
        RawAddressBook("123", List(RawPerson("456", Some("kek"), "+79425047642"), RawPerson("cheburek", None, "123"))),
        allErrors.drop(4)
      )
      check(
        RawAddressBook("123", List(RawPerson("456", Some("kek"), "+79425047642"), RawPerson("100500", None, "123"))),
        allErrors.drop(5)
      )
      check(
        RawAddressBook(
          "123",
          List(RawPerson("456", Some("kek"), "+79425047642"), RawPerson("100500", Some("nikolai"), "123"))
        ),
        allErrors.drop(6)
      )
    }
  }
}
