package adt

object Example01 extends App {
  // Модификатор sealed - наследование возможно только в рамках текущего файла
  sealed trait Pet

  // case classes - since 2007
  case class Dog(id: Long, name: String) extends Pet

  case class Cat(id: Long, name: String, color: String) extends Pet

  val dog = Dog(1, "Doggie")
  val cat = Cat(2, "Cattie", "Black")

  val dog2 = Dog(1, "Doggie")
  val cat2 = Cat.apply(2, "Cattie", "Black")

  println(dog.name) // 1

  println(dog) // Dog(1,Doggie)

  println(cat) // Cat(2,Cattie,Black)

  println(cat == cat2) // true

  val pets: List[Pet] = List(dog, cat)

  println(pets) // List(Dog(1,Doggie), Cat(2,Cattie,Black))
}
