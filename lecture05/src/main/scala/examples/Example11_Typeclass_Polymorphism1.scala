package examples

import scala.language.implicitConversions

object Example11_Typeclass_Polymorphism1 extends App {

  // сущности, представляющие данные
  case class Circle(radius: Double)
  case class Rectangle(width: Double, length: Double)

  // тайпкласс
  trait Area[A] {
    def area(a: A): Double
  }

  // сущности, отвечающие за реализацию
  object CircleArea extends Area[Circle] {
    override def area(circle: Circle): Double = math.Pi * math.pow(circle.radius, 2)
  }

  object RectangleArea extends Area[Rectangle] {
    override def area(rectangle: Rectangle): Double = rectangle.width * rectangle.length
  }

  // Обобщенная функция
  def areaOf[A](shape: A, area: Area[A]): Double = area.area(shape)

  areaOf(Circle(11), CircleArea)
  areaOf(Rectangle(12, 15), RectangleArea)
}
