package registry.domain.model

import scala.util.control.NoStackTrace

sealed trait ValidationError extends Exception with NoStackTrace

object ValidationError {

  type PhoneIsInvalid = PhoneIsInvalid.type
  case object PhoneIsInvalid extends ValidationError

  type NameHasInvalidCharacters = NameHasInvalidCharacters.type
  case object NameHasInvalidCharacters extends ValidationError

  type SurnameHasInvalidCharacters = SurnameHasInvalidCharacters.type
  case object SurnameHasInvalidCharacters extends ValidationError

  type PatronymicHasInvalidCharacters = PatronymicHasInvalidCharacters.type
  case object PatronymicHasInvalidCharacters extends ValidationError

  type PassportIsInvalid = PassportIsInvalid.type
  case object PassportIsInvalid extends ValidationError
}
