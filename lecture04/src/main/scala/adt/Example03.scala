package adt

object Example03 extends App {
  sealed trait Pet

  sealed trait Color
  object Color {
    case object Red   extends Color
    case object Black extends Color
    case object White extends Color
  }

  // Exhaustive pattern matching
  def showColor(color: Color): String =
    color match {
      case Color.Red   => "RED"
      case Color.Black => "BLACK"
      case Color.White => "WHITE"
    }

  case class Dog(id: Long, name: String) extends Pet

  case class Cat(id: Long, name: String, color: Color) extends Pet

  val dog: Dog = Dog(1, "Doggie")
  val cat: Cat = Cat(2, "Cattie", Color.Black)

  // Exhaustive pattern matching
  def color(pet: Pet): Color =
    pet match {
      case Cat(_, _, color) => color
      case _: Dog           => Color.Red
    }

  // Определенный компилятором метод copy
  val whiteCat: Cat = cat.copy(id = 3, color = Color.White)

  println(color(dog)) // Red

  println(color(cat)) // Black

  println(color(whiteCat)) // White

  println(showColor(Color.Black)) // BLACK

}
