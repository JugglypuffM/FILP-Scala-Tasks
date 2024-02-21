// классы и объекты

class Vehicle(description: String) {
  println("constructing...")
  val name = description + " vehicle"
  def print(): Unit = println(name)
}

class Car extends Vehicle("heavy")

val car = new Car

car.print()

object FastCar extends Vehicle("fast")

FastCar.print()

trait Flight {
  def wings: String
  def fly(): String =
    "Хлопаю моими " + wings
}

trait Bird {
  def wings: String =
    "крыльями"
}

class Pigeon extends Bird with Flight

new Pigeon().fly()
class Penguin extends Bird


//new Penguin().fly() // value лететь is not a member of Пингвин

class Dragon extends Flight {
  override def wings: String =
    "стеклянными крыльями"
}

new Dragon().fly()

object Icarus extends Flight {
  def wings: String =
    "восковыми крыльями"
}

Icarus.fly()

Icarus.fly()

Icarus.fly()
