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
             profitMargin: Float)

case class BookingForNormalization(
            name: String,
            city: String,
            country: String,
            price: Float,
            discount: Float,
            profitMargin: Float,
            numberOfVisitors: Int)

case class BookingScore(
             name: String,
             city: String,
             country: String,
             priceScore: Float,
             discountScore: Float,
             profitMarginScore: Float,
             numberOfVisitorScore: Float)
