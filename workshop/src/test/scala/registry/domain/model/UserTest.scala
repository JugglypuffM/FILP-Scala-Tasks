package registry.domain.model

import cats.data.NonEmptyChainImpl
import cats.data.Validated._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import registry.domain.model.ValidationError._

class UserTest extends AnyFreeSpec with Matchers {

  "User.apply" - {
    "should" - {
      "create user when params is valid" in {
        val usr = User("Иван", "Иванов", Some("Иванович"), "1234567890", "+79993332222")
        usr shouldBe Valid(User("Иван", "Иванов", Some("Иванович"),
                                 Passport(Passport.Serial("1234"), Passport.Number("567890")),
                                 PhoneNumber("+7", "999", "3332222")))
      }

      "accumulate errors when params is invalid" in {
        User("Ivan", "Ivanov", Some("Ivanovich"), "1", "1") shouldBe
          Invalid(NonEmptyChainImpl(
            NameHasInvalidCharacters, SurnameHasInvalidCharacters,
            PatronymicHasInvalidCharacters, PassportIsInvalid, PhoneIsInvalid))
      }
    }
  }
}
