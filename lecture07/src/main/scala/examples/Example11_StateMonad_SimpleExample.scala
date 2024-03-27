package examples

import examples.data.State
import examples.data.State._
import examples.typeclasses.MonadSyntax._

object Example11_StateMonad_SimpleExample extends App {

  def updateStateExample(str: String): State[Int, String] =
    for {
      current <- State.ask[Int, Int](identity)
      _ <- str match {
        case "increment" => State.update[Int](_ + 1)
        case "decrement" => State.update[Int](_ - 1)
        case _           => State.update[Int](identity) // do nothing
      }
      updated <- State.ask[Int, Int](identity)
    } yield if (current != updated) "updated" else "unchanged"

  val (res0, state0) = updateStateExample("increment").run(100)
  println(s"res=$res0, state=$state0")
  // res=updated, state=101

  val (res1, state1) = updateStateExample("decrement").run(100)
  println(s"res=$res1, state=$state1")
}
