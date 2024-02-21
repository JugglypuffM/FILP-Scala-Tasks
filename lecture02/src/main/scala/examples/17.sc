// Option

case class User(firstname: String, lastname: String)

def findUser(login: String): User = {
  null
}

def findUserOption(login: String): Option[User] = {
  None
}

val ivan = findUser("ivan")

//ivan.firstname

if (ivan != null) ivan.firstname


val petr = findUserOption("petr")

//petr.firstname

val petrName = petr.map(user => user.firstname)

petrName.getOrElse("Petr not found")
