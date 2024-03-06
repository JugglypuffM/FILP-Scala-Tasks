package my.pakage

object Main2 {
  def main(args: Array[String]): Unit = {
    import another.pakage.Data
    val d = new Data(1, 2)
    println("Hello world")
  }
}
