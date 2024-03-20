package examples

import examples.Example06_Applicative_Eval.{Eval, Temperature}
import examples.typeclasses.ApplicativeInstances._
import examples.typeclasses.{Applicative, Traverse}
import examples.typeclasses.TraverseInstances._
import examples.typeclasses.TraverseSyntax._

object Example07_Traverse extends App {

  def getTemperature[F[_]: Applicative](city: String): F[Temperature] = {
    city match {
      case "Moscow"        => Applicative[F].pure(Temperature(-5))
      case "Yekaterinburg" => Applicative[F].pure(Temperature(-10))
      case "Sochi"         => Applicative[F].pure(Temperature(15))
    }
  }

  val cities: List[String]             = List("Moscow", "Yekaterinburg", "Sochi")
  val temps: Option[List[Temperature]] = cities.traverse(city => getTemperature[Option](city))
  println(temps)
}
