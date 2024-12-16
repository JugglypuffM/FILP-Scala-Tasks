package registry.domain.model

import cats.data.Validated._
import cats.data.ValidatedNec
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import registry.domain.model.ValidationError.PassportIsInvalid

case class Passport private (serial: Passport.Serial, number: Passport.Number)

object Passport {
  private val passportRegex = raw"(\d{4})(\d{6})".r

  def parse(raw: String): ValidatedNec[PassportIsInvalid, Passport] =
    raw match {
      case passportRegex(s, n) => Passport(Passport.Serial(s), Passport.Number(n)).validNec
      case _                   => PassportIsInvalid.invalidNec
    }

  @SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
  def parseUnsafe(str: String): Passport = parse(str) match {
    case Valid(p)     => p
    case Invalid(err) => throw err.head
  }

  case class Serial(value: String) extends AnyVal

  case class Number(value: String) extends AnyVal

}
