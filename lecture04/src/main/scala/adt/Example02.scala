package adt

object Example02 extends App {
  // val у поля класса делает поле публичным
  class Dog(val id: Int, val name: String) {

    override def toString: String = s"Dog($id, $name)"

    override def equals(obj: Any): Boolean =
      if (obj == this) true
      else if (obj != null && obj.getClass == this.getClass) {
        val other = obj.asInstanceOf[Dog]

        other.id == id && other.name == this.name
      } else false

    override def hashCode(): Int = java.util.Objects.hash(id, name)
  }

  object Dog {
    // Создание класса
    def apply(id: Int, name: String): Dog =
      new Dog(id, name)

    // Набор полей класса
    def unapply(dog: Dog): Option[(Int, String)] =
      Some((dog.id, dog.name))
  }

  val dog: Dog = Dog(1, "Doggie")

  println(dog.name) // Doggie

  println(dog == Dog(1, "Doogie")) // true

  def doogie(dog: Dog): Long =
    dog match {
      case Dog(id, "Doggie") => id
      case Dog(id, _)        => id + 1
    }

  println(doogie(dog)) // 1

}
