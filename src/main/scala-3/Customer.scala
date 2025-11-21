case class Gender(value: String)

case class Customer(
                     customerId: String,
                     gender: Gender,
                     age: Int,
                     originalCountry: String,
                     state: String,
                     location: String
                   )
