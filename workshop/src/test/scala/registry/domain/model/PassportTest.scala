package registry.domain.model

import cats.data.NonEmptyChainImpl
import cats.data.Validated._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import registry.domain.model.ValidationError.PassportIsInvalid

class PassportTest extends AnyFreeSpec with Matchers {
  "Passport.parse" - {
    "should" - {
      "parse passport from string" in {
        Passport.parse("1234567890") shouldBe Valid(Passport(Passport.Serial("1234"), Passport.Number("567890")))
      }
      "return error when phone is incorrect" in {
        Passport.parse("1") shouldBe Invalid(NonEmptyChainImpl(PassportIsInvalid))
      }
    }
  }
}
