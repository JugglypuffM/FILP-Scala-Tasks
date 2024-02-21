// паттерн матчинг

sealed trait Pet {
  val name: String
  val age: Int
}

case class Cat(name: String, age: Int, color: String) extends Pet {
  def meow(): String = "meow-meow"
}

case class Dog(name: String, age: Int, breed: String) extends Pet {
  def bark(): String = "woof-woof"
}

val pet: Pet = Dog("Carlos", 6, "spaniel")

pet match {
  case Cat(name, age, color) => println(s"${name} is ${age} years old and is ${color}")
  case Dog(name, age, breed) => println(s"${breed.capitalize} ${name} is ${age} years old")
}
