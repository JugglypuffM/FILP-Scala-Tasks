package exercises04.parser

import exercises04.either.EitherCombinators._
import Error._

object Examples {
  private val passportRegex = raw"(\d{4}) (\d{6})".r

  private def getUserName(rawUser: RawUser): Option[UserName] =
    for {
      firstName <- rawUser.firstName
      secondName <- rawUser.secondName
    } yield UserName(firstName, secondName, rawUser.thirdName)
  private def getPassport(rawUser: RawUser): Option[Passport] = rawUser.passport match {
    case passportRegex(s, n) => for {
      series <- s.toLongOption
      number <- n.toLongOption
    }yield Passport(series, number)
    case _ => None
  }

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
      username <- getUserName(rawUser)
      passport <- getPassport(rawUser)
      id <- rawUser.id.toLongOption
      _ <- if (rawUser.banned == "false") Some() else None
    } yield User(id, username, passport)

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
      id <- Either.fromOption(rawUser.id.toLongOption)(InvalidId)
      username <- Either.fromOption(getUserName(rawUser))(InvalidName)
      passport <- Either.fromOption(getPassport(rawUser))(InvalidPassport)
    } yield User(id, username, passport)
}
