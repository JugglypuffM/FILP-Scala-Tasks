object f01_callbacks extends App {

  def printWithCallback(s: String)(callback: () => Unit): Unit = {
    println(s)
    callback()
  }

  // Необходимо связать 3 задачи:
  // 1) Напечатать "Hello"
  // 2) Напечатать "Call"
  // 3) Напечатать "Backs"

  printWithCallback("Call") {
    () => printWithCallback("Back") {
      () => printWithCallback("Hell") {
        () => ()
      }
    }
  }


}
