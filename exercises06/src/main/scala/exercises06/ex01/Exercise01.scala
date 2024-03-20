package exercises06.ex01

import exercises06.data.NonEmptyList
import exercises06.typeclasses._
import exercises06.typeclasses.{Applicative, Traverse}

object Exercise01 {
  object Syntax {}

  object Instances {
    import Syntax._

    implicit val strMonoid = ???

    implicit val intMonoid = ???

    implicit val listInstances: Traverse[List] with Applicative[List] = ???

    implicit val optionInstances: Traverse[Option] with Applicative[Option] = ???

    implicit val nelInstances: Traverse[NonEmptyList] with Applicative[NonEmptyList] = ???

    implicit def listMonoid[A] = ???
  }
}
