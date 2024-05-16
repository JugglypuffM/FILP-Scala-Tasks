package registry.domain.model

import cats.data.Validated._
import cats.data.ValidatedNec
import cats.syntax.all._
import registry.domain.model.ValidationError.PhoneIsInvalid

import scala.util.matching.Regex

case class PhoneNumber private (country: String, code: String, number: String)

object PhoneNumber {
  private val phoneRegex: Regex = raw"\+?(\d)(\d{3})(\d{7})".r

  //TODO: сделать парсинг номера телефона
  def parse(raw: String): ValidatedNec[PhoneIsInvalid, PhoneNumber] = raw match {
    case phoneRegex(country, code, number) => PhoneNumber(country, code, number).validNec
    case _ => PhoneIsInvalid.invalidNec
  }
  @SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
  def parseUnsafe(str: String): PhoneNumber = parse(str) match {
    case Valid(p)      => p
    case Invalid(errs) => throw errs.head
  }
}
