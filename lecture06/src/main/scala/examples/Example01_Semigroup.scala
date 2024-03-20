package examples

import examples.typeclasses.Semigroup
import examples.typeclasses.SemigroupInstances._
import examples.typeclasses.SemigroupSyntax._

import scala.language.implicitConversions

object Example01_Semigroup extends App {

  Semigroup[Int].combine(1, 2)
  // result: 3

  val resultForInt: Int = 1 |+| 2 |+| 3
  println(resultForInt)
  // result: 6

  val resultForStrings: String = "Hello" |+| " " |+| "world!"
  println(resultForStrings)
  // result: Hello world!

  val resultForList: List[Int] = List(1, 2) |+| List(3, 4) |+| Nil
  println(resultForList)
  // result: List(1, 2, 3, 4)

  // Laws

  // Associativity
  val result: Boolean = ((1 |+| 2) |+| 3) == (1 |+| (2 |+| 3))
  println(result) // True

  // TODO: ???
  def combineAll[A: Semigroup](list: List[A]): A = {
    ???
  }
}
