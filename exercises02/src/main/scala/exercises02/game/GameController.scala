package exercises02.game

import scala.io.StdIn

trait GameController {
  def nextLine(): String

  def askNumber(): Unit

  def numberIsBigger(): Unit

  def numberIsSmaller(): Unit

  def guessed(): Unit

  def giveUp(number: Int): Unit

  def wrongInput(): Unit
}

object GameController {
  final val IGiveUp: String = "I give up"

  val live: GameController = new GameController {
    def nextLine(): String =
      StdIn.readLine()

    def askNumber(): Unit =
      Console.out.println("Guess the number:")

    def numberIsBigger(): Unit =
      Console.out.println("My number is bigger")

    def numberIsSmaller(): Unit =
      Console.out.println("My number is smaller")

    def guessed(): Unit =
      Console.out.println("GG")

    def giveUp(number: Int): Unit =
      Console.out.println(s"My number is $number")

    def wrongInput(): Unit =
      Console.out.println("Wrong input")
  }
}
