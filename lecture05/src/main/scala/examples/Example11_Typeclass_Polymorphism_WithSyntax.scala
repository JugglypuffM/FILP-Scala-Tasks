package examples

import scala.language.implicitConversions

object Example11_Typeclass_Polymorphism_WithSyntax extends App {

  // сущности, представляющие данные
  case class Circle(radius: Double)

  case class Rectangle(width: Double, length: Double)

  // тайпкласс
  trait Area[A] {
    def area(a: A): Double
  }

  // сущности, отвечающие за реализацию
  implicit val circleArea: Area[Circle] = new Area[Circle] {
    override def area(circle: Circle): Double = math.Pi * math.pow(circle.radius, 2)
  }

  implicit val rectangleArea: Area[Rectangle] = new Area[Rectangle] {
    override def area(rectangle: Rectangle): Double = rectangle.width * rectangle.length
  }

  // Синтаксис
  implicit class AreaSyntax[A](private val figure: A) extends AnyVal {
    def area(implicit area: Area[A]): Double = area.area(figure)
  }

  Circle(42).area
  Rectangle(12, 15).area
}
