package lecture

object Example06NamedArguments extends App {

  def saveImage(format: String = "jpg", width: Int, height: Int): Unit = println("saving image")

//  saveImage(1920, 1080)

  saveImage("jpg", 1920, 1080)

  saveImage(width = 1920, height = 1080)

  saveImage(height = 1080, width = 1920)

  saveImage(height = 1080, width = 1920, format = "png")
}
