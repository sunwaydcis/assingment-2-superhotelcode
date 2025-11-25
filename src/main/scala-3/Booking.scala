import java.time.{LocalDate, LocalTime}

class Booking(
             val bookingId: String,
             val bookingDate: LocalDate,
             val bookingTime: LocalTime,
             val customer: Customer,
             val destinationCountry: String,
             val destinationCity: String,
             val noOfPeople: Int,
             val checkInDate: LocalDate,
             val noOfDays: Int,
             val checkoutDate: LocalDate,
             val room: Int,
             val hotel: Hotel,
             val payment: Payment,
             val bookingPrice: Float,
             val discount: Float,
             val gst: Float,
             val profitMargin: Float
             ):
  // Used for calculating profit score
  private def discountedPrice: Float = bookingPrice * (1 - discount/100)
  // Used for evaluating cost-effective
  // Lowest value -> cost-effective for customers
  // Highest score -> benefits for hotel side
  def profitScore: Float = discountedPrice * profitMargin
