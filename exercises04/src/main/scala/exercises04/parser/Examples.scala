package exercises04.parser

import exercises04.either.EitherCombinators._
import Error._

import scala.Some

object Examples {
  private val splitRegex    = " ".r
  private val passportRegex = raw"\d{4} \d{6}".r

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть None
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть None
    * если rawUser.id не парсится в Long то функция должна вернуть None
    * rawUser.banned не "false", вернуть None
    * используйте for-comprehension
    */
  def transformToOption(rawUser: RawUser): Option[User] =
    for {
      _ <- if (rawUser.firstName.isDefined && rawUser.secondName.isDefined) Some() else None
      _ <- if (passportRegex.matches(rawUser.passport)) Some() else None
      _ <- if (rawUser.banned == "false") Some() else None
      _ <- rawUser.id.toLongOption match {
        case Some(_) => Some()
        case None    => None
      }
    } yield User(
      rawUser.id.toLong,
      UserName(rawUser.firstName.get, rawUser.secondName.get, rawUser.thirdName),
      Passport(splitRegex.split(rawUser.passport)(0).toLong, splitRegex.split(rawUser.passport)(1).toLong)
    )

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть Left(InvalidName)
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть Left(InvalidPassport)
    * если rawUser.id не парсится в Long то функция должна вернуть Left(InvalidId)
    * если rawUser.banned не "true" или "false" - вернуть Left(InvalidBanned). Если "true" - вернуть Left(Banned).
    * у ошибок есть приоритет:
    * 1. InvalidBanned
    * 2. Banned
    * 2. InvalidId
    * 3. InvalidName
    * 4. InvalidPassport
    * используйте for-comprehension
    * но для того, чтобы for-comprehension заработал надо реализовать map и flatMap в Either
    */
  def transformToEither(rawUser: RawUser): Either[Error, User] =
    for {
      _ <- if (rawUser.banned == "false") Right()
      else if (rawUser.banned == "true") Left(Banned)
      else Left(InvalidBanned)
      _ <- rawUser.id.toLongOption match {
        case Some(_) => Right()
        case None    => Left(InvalidId)
      }
      _ <- if (rawUser.firstName.isDefined && rawUser.secondName.isDefined) Right() else Left(InvalidName)
      _ <- if (passportRegex.matches(rawUser.passport)) Right() else Left(InvalidPassport)
    } yield User(
      rawUser.id.toLong,
      UserName(rawUser.firstName.get, rawUser.secondName.get, rawUser.thirdName),
      Passport(splitRegex.split(rawUser.passport)(0).toLong, splitRegex.split(rawUser.passport)(1).toLong)
    )
}
