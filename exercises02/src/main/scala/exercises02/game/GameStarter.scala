package exercises02.game

import scala.util.Random

object GameStarter extends App {
  val number: Int = Random.nextInt(10) + 1
  new Game(GameController.live).play(number)
}
