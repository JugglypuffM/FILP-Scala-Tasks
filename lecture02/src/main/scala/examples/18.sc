// option for-comprehension

case class Address(street: String, city: String)

case class User(
    firstname: String,
    lastname: String,
    surname: Option[String],
    age: Int,
    address: Option[Address],
    phone: Option[String],
    mobilePhone: Option[String]
)

def findUser(login: String): Option[User] =
  Some(
    User(
      "Иван",
      "Петров",
      Some("Сергеевич"),
      19,
      Some(Address("Ленина", "Екатеринбург")),
      Some("+73431234567"),
      Some("+79123456789")
    )
  )

for {
  user <- findUser("mylogin") if user.age >= 18
  surname <- user.surname
  address <- user.address
  phone <- user.phone
  mobilePhone <- user.mobilePhone
} yield s"${user.firstname} ${surname} живет на улице ${address.street} и его домашний номер $phone и мобильный номер $mobilePhone"

findUser("mylogin")
  .filter(_.age >= 18)
  .flatMap(user =>
    user.surname.flatMap(surname =>
      user.address.flatMap(address =>
        user.phone.flatMap(phone =>
          user.mobilePhone.map(mobilePhone =>
            s"${user.firstname} ${surname} живет на улице ${address.street} и его домашний номер $phone и мобильный номер $mobilePhone"
          )
        )
      )
    )
  )

findUser("mylogin")
  .filter(_.age >= 18)
  .flatMap(user =>
    user.surname.flatMap(surname =>
      user.address.flatMap(address =>
        user.phone.flatMap(phone =>
          user.mobilePhone.map(mobilePhone =>
            s"${user.firstname} ${surname} живет на улице ${address.street} и его домашний номер $phone и мобильный номер $mobilePhone"
          )
        )
      )
    )
  )
