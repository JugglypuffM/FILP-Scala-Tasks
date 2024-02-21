// AnyVal Value классы

case class User(firstname: String, lastname: String)

val firstname = "Иван"
val lastname = "Петров"

User(firstname, lastname)
User(lastname, firstname)

case class FirstName(value: String) extends AnyVal

case class LastName(value: String) extends AnyVal

case class TypedUser(firstname: FirstName, lastname: LastName)

val typedFirstname = FirstName("Иван")
val typedLastname = LastName("Петров")

TypedUser(typedFirstname, typedLastname)

// именованные аргументы
TypedUser(lastname = typedLastname, firstname = typedFirstname)

//TypedUser(typedLastname, typedFirstname)


