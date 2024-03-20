package examples

import examples.Example06_Applicative_Eval.Temperature
import examples.typeclasses.ApplicativeError
import examples.typeclasses.ApplicativeErrorInstances._
import examples.typeclasses.ApplicativeSyntax._

import scala.util.Try

object Example08_ApplicativeError extends App {

  def getTemperature[F[_]](city: String)(implicit ae: ApplicativeError[F, Throwable]): F[Temperature] = {
    city match {
      case "Moscow"        => Temperature(-5).pure[F]
      case "Yekaterinburg" => Temperature(-10).pure[F]
      case "Sochi"         => Temperature(15).pure[F]
      case unknown         => ApplicativeError[F, Throwable].raiseError(new Throwable(s"City $unknown wasn't found"))
    }
  }

  // Try examples
  val moscow: Try[Temperature] = getTemperature[Try]("Moscow")
  println(moscow)

  val sochi: Try[Temperature] = getTemperature[Try]("Sochi")
  println(sochi)

  val kaliningrad: Try[Temperature] = getTemperature[Try]("Kaliningrad")
  println(kaliningrad)

  val allCitiesSuccess: Try[(Temperature, Temperature, Temperature)] = (
    getTemperature[Try]("Moscow"),
    getTemperature[Try]("Sochi"),
    getTemperature[Try]("Yekaterinburg")
  ).mapN {
    case (msk, sch, yek) => (msk, sch, yek)
  }

  println(allCitiesSuccess)

  val allCitiesFail: Try[(Temperature, Temperature, Temperature)] = (
    getTemperature[Try]("Moscow"),
    getTemperature[Try]("Sochi"),
    getTemperature[Try]("Kaliningrad")
  ).mapN {
    case (msk, sch, k) => (msk, sch, k)
  }

  println(allCitiesFail)

  // Either examples
  type EitherTh[A] = Either[Throwable, A]

  val moscowEither: EitherTh[Temperature] = getTemperature[EitherTh]("Moscow")
  println(moscowEither)

  val sochiEither: EitherTh[Temperature] = getTemperature[EitherTh]("Sochi")
  println(sochiEither)

  val kaliningradEither: EitherTh[Temperature] = getTemperature[EitherTh]("Kaliningrad")
  println(kaliningradEither)

  val allCitiesEither: Try[(Temperature, Temperature, Temperature)] = (
    getTemperature[Try]("Moscow"),
    getTemperature[Try]("Sochi"),
    getTemperature[Try]("Kaliningrad")
  ).mapN {
    case (msk, sch, klngrd) => (msk, sch, klngrd)
  }

  println(allCitiesEither)
}
