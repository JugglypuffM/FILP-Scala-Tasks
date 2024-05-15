package registry.adapter.jdbc.sqlite

import cats.effect._
import cats.syntax.all._
import doobie._
import doobie.implicits._
import registry.domain.model.{Passport, PhoneNumber, User}
import registry.domain.service.UserAlg

object UserAlg {
  def build[F[_]: MonadCancelThrow](transactor: Transactor[F]): F[UserAlg[F]] =
    for {
      _ <- createTable.update.run.transact(transactor)
      alg = new UserAlgImpl[F](transactor)
    } yield alg

  private class UserAlgImpl[F[_]: MonadCancelThrow](transactor: Transactor[F]) extends UserAlg[F] {

    override def getBy(passport: Passport): F[Option[User]] =
      sql"""
      SELECT passport, phone, name, surname, patronymic
      FROM user 
      WHERE passport = ${passport.serial.value + passport.number.value}
    """.query[(String, String, String, String, Option[String])]
        .to[List]
        .transact(transactor)
        .map(_.headOption.map {
          case (passport, phone, name, surname, patronymic) =>
            User(
              name = name,
              surname = surname,
              patronymic = patronymic,
              passport = Passport.parseUnsafe(passport),
              phoneNumber = PhoneNumber.parseUnsafe(phone)
            )
        })

    override def persist(user: User): F[Unit] = sql"""
      INSERT INTO user (passport, phone, name, surname, patronymic) VALUES (
        ${user.passport.serial.value + user.passport.number.value},
        ${user.phoneNumber.country + user.phoneNumber.code + user.phoneNumber.number},
        ${user.name},
        ${user.surname},
        ${user.patronymic}
      )
    """.update.run.transact(transactor).as(())

  }

  private val createTable = sql"""
    CREATE TABLE IF NOT EXISTS user (
      passport TEXT NOT NULL UNIQUE,
      phone TEXT NOT NULL UNIQUE,
      name TEXT NOT NULL,
      surname TEXT NOT NULL,
      patronymic TEXT
    )
  """

}
