package exercises02.game

class Game(controller: GameController) {

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
  def play(number: Int): Unit = {
    while (true){
      controller.askNumber()
      val line = controller.nextLine()
      if (line.equals(GameController.IGiveUp)) {controller.giveUp(number); return }
      else {
        try{
          val suggestion = line.toInt
          if (suggestion.equals(number)) {controller.guessed(); return}
          else if (suggestion < number) controller.numberIsBigger()
          else controller.numberIsSmaller()
        }
        catch {
          case _: Exception => controller.wrongInput()
        }
      }
    }
  }
}
