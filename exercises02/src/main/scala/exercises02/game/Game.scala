package exercises02.game

import scala.annotation.tailrec

class Game(controller: GameController) {
  private object Nextline{
    def unapply(line: String): Option[Int] = {
      try{line.toIntOption}
      catch {case _: Exception => None}
    }
  }
  /**
    * Игра угадай число
    * Ввод и вывод необходимо осуществлять с помощью методов controller
    *
    * Игра должна вызывать controller.askNumber перед каждой попыткой игрока угадать число
    * И вызвать controller.nextLine для получения ввода игрока
    * Если игрок ввел число меньше загаданного, игра должна вызвать controller.numberIsBigger
    * Если игрок ввел число больше загаданного, игра должна вызвать controller.numberIsSmaller
    * Если игрок угадал число, игра должна закончиться и вызвать controller.guessed
    * Если игрок написал GameController.IGiveUp, игра должна закончиться и вызвать controller.giveUp(number)
    * Если игрок ввел неизвестную комбинацию символов, надо вызвать contoller.wrongInput и продолжить игру
    *
    * @param number загаданное число
    */
  @tailrec
  final def play(number: Int): Unit = {
    controller.askNumber()
    controller.nextLine() match {
      case Nextline(value) if value == number => controller.guessed()
      case Nextline(value) if value > number => controller.numberIsSmaller(); play(number)
      case Nextline(value) if value < number => controller.numberIsBigger(); play(number)
      case GameController.IGiveUp => controller.giveUp(number)
      case _ => controller.wrongInput(); play(number)
    }
  }
}
