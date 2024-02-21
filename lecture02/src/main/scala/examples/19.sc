// паттерн матчинг

trait Pet

class Cat extends Pet {
  def meow(): String = "meow-meow"
}

class Dog extends Pet {
  def bark(): String = "woof-woof"
}

val pet: Pet = new Cat()

pet match {
  case cat: Cat => cat.meow()
  case dog: Dog => dog.bark()
}

val option: Option[Int] = Some(12)

option match {
  case Some(value) => println(s"value is ${value}!")
  case None        => println("no value provided")
}

val option2 = Option("123")

val a = option2.flatMap(value => value.toIntOption)
