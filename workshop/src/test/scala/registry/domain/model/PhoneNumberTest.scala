package registry.domain.model

import cats.data.NonEmptyChainImpl
import cats.data.Validated._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import registry.domain.model.ValidationError.PhoneIsInvalid

class PhoneNumberTest extends AnyFreeSpec with Matchers {
  "PhoneNumber.parse " - {
    "should " - {
      "parse phone number from string" in {
        PhoneNumber.parse("+79993332222") shouldBe Valid(PhoneNumber("+7", "999", "3332222"))
        PhoneNumber.parse("89993332222") shouldBe Valid(PhoneNumber("8", "999", "3332222"))
        PhoneNumber.parse("+7 999  3332222") shouldBe Valid(PhoneNumber("+7", "999", "3332222"))
      }

      "return error when phone is incorrect" in {
        PhoneNumber.parse("99993332222") shouldBe Invalid(NonEmptyChainImpl(PhoneIsInvalid))
      }
    }
  }
}
