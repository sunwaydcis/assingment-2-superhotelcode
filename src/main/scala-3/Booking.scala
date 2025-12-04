import java.time.{LocalDate, LocalTime}

case class Booking(
             bookingId: String,
             bookingDate: LocalDate,
             bookingTime: LocalTime,
             customer: Customer,
             destinationCountry: String,
             destinationCity: String,
             noOfPeople: Int,
             checkInDate: LocalDate,
             noOfDays: Int,
             checkoutDate: LocalDate,
             room: Int,
             hotel: Hotel,
             payment: Payment,
             bookingPrice: Float,
             discount: Float,
             gst: Float,
             profitMargin: Float
             )
