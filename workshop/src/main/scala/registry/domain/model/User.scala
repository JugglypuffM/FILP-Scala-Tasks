package registry.domain.model

import cats.data.ValidatedNec
import cats.syntax.all._
import registry.domain.model.ValidationError._

case class User private[model] (
    name: String,
    surname: String,
    patronymic: Option[String],
    passport: Passport,
    phoneNumber: PhoneNumber
)

object User {
  private type ValidationResult[A] = ValidatedNec[ValidationError, A]

  private val nonEmptyRuStrReg = "[а-яА-Я]+".r

  private def validateName(name: String): ValidationResult[String] = {
    if (nonEmptyRuStrReg.matches(name)) name.validNec
    else NameHasInvalidCharacters.invalidNec
  }

  private def validateSurname(surname: String): ValidationResult[String] = {
    if (nonEmptyRuStrReg.matches(surname)) surname.validNec
    else SurnameHasInvalidCharacters.invalidNec
  }

  private def validatePatronymic(patronymic: Option[String]): ValidationResult[Option[String]] = patronymic match {
    case Some(p) =>
      if (nonEmptyRuStrReg.matches(p)) p.some.validNec
      else PatronymicHasInvalidCharacters.invalidNec
    case None => None.validNec
  }

  def apply(
      name: String,
      surname: String,
      patronymic: Option[String],
      passport: String,
      phoneNumber: String
  ): ValidationResult[User] =
    (
      validateName(name),
      validateSurname(surname),
      validatePatronymic(patronymic),
      Passport.parse(passport),
      PhoneNumber.parse(phoneNumber)
    ).mapN(User.apply)
}
