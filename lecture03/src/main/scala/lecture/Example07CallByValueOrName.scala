package lecture

object Example07CallByValueOrName extends App {

  def callByValue(x: Long): Unit = {
    println("call by value " + x)
    println("call by value " + x)
  }

  def callByName(x: => Long): Unit = {
    println("call by name " + x)
    println("call by name " + x)
  }

  callByValue(System.nanoTime())
  callByName(System.nanoTime())



  def callByValue2(f: Unit): Unit = {
    f
    f
  }

  def callByName2(f: => Unit): Unit = {
    f
    f
  }

  println()

//  callByValue2(println("kek"))

  println()

//  callByName2(println("kek"))
}
