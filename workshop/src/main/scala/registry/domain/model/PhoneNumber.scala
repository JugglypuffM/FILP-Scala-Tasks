package registry.domain.model

import cats.data.Validated._
import cats.data.ValidatedNec
import cats.syntax.all._
import registry.domain.model.ValidationError.PhoneIsInvalid

case class PhoneNumber private (country: String, code: String, number: String)

object PhoneNumber {

  //TODO: сделать парсинг номера телефона
  def parse(raw: String): ValidatedNec[PhoneIsInvalid, PhoneNumber] = ???

  @SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
  def parseUnsafe(str: String): PhoneNumber = parse(str) match {
    case Valid(p)      => p
    case Invalid(errs) => throw errs.head
  }
}
