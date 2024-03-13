package examples

import scala.language.implicitConversions

object Example10_Subtyping extends App {

  trait Area {
    def area: Double
  }

  class Circle(radius: Double) extends Area {
    override def area: Double = math.Pi * math.pow(radius, 2)
  }

  class Rectangle(width: Double, length: Double) extends Area {
    override def area: Double = width * length
  }

  // Обобщенная функция
  def areaOf(area: Area): Double = area.area

  println(areaOf(new Circle(10)))
  println(areaOf(new Rectangle(5, 5)))
}
