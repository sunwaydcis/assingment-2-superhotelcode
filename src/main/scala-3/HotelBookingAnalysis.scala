import scala.collection.View

// Parent print class for every case such as
// highest booking country, economist hotel, and profitable hotel
trait IPrintAnalysis

// Use parametric to reuse for multiple classes (future purposes), not only hotel booking analysis
trait IAnalysis[T]:
  // Show analysis information
  def showAnalysis(content: IPrintAnalysis): Unit

  // Normalization based on min and max
  def normalizeValueByRange(value: Float, min: Float, max: Float): Float =
    // Avoid dividing by zero
    if min == max then return 0.0
    (value - min) / (max - min)

// Used for storing min and max value for normalization
// Int / Float
case class NormalizationRange[T1](min: T1, max: T1)

// Used for showing analysis information on each case
case class HighestBookingCountry(name: String, numberOfBookings: Int) extends IPrintAnalysis

case class EconomistHotel(
                            name: String,
                            city: String,
                            country: String,
                            avgScore: Float,
                            priceScore: Float,
                            discountScore: Float,
                            profitMarginScore: Float) extends IPrintAnalysis

case class ProfitableHotel(
                            name: String,
                            city: String,
                            country: String,
                            avgScore: Float,
                            numberOfVisitorScore: Float,
                            profitMarginScore: Float) extends IPrintAnalysis

class HotelBookingAnalysis extends CsvUtil[Booking], IAnalysis[Booking]:
  private var analysisDataList: List[Booking] = List.empty

  override def readCsv(filename: String): List[Booking] =
    val dataList = super.readCsv(filename)
    analysisDataList = dataList

    // Assign local var to proceed with aggregation
    analysisDataList

  override def parseCsvData(data: List[String]): Booking =
    Booking(
      data.head,
      DateUtil.parseDate(data(1)),
      // Avoid error when parsing with having AM or PM in value
      DateUtil.parseTime(data(2)),
      Customer(data(3), Gender(data(4)), data(5).toInt, data(6), data(7), data(8)),
      data(9),
      data(10),
      data(11).toInt,
      DateUtil.parseDate(data(12)),
      data(13).toInt,
      DateUtil.parseDate(data(14)),
      data(15).toInt,
      Hotel(data(16), data(17).toFloat),
      Payment(PaymentMode(data(18)), data(19)),
      data(20).toFloat,
      // Remove percentage sign from value
      // Convert percentage to decimal
      data(21).replace("%", "").toFloat / 100,
      data(22).toFloat,
      data(23).toFloat
    )

  // Get country has the highest number of booking
  def getHighestBookingCountry: HighestBookingCountry =
    // Group by destination country and count number of booking on each country
    val numberOfBookingPerCountryList = analysisDataList
      // Group by a specific key such as destination_country and count on each value
      .groupMapReduce(_.destinationCountry)(_ => 1)(_ + _)
      // Convert result into HighestBookingCountry
      .map(b => HighestBookingCountry(b._1, b._2))

    // Get the highest number of bookings
    numberOfBookingPerCountryList.maxBy(_.numberOfBookings)

  // Get normalization data for getting economist or profitable hotel
  // No parse to list to reduce memory and enhance performance
  def getNormalizationData: View[BookingScore] =
    // Get avg value on each hotel (unique hotel: differs from country and city as well)
    val bookingDataForNormalization = analysisDataList.groupBy(
      b => (b.hotel.hotelName, b.destinationCity, b.destinationCountry)
    ).view.mapValues(bookings => {
      // Avoid loop many times when using map and sum on each field
      // Use foreach loop one times
      var sumPrice: Float = 0
      var sumDiscount: Float = 0
      var sumProfitMargin: Float = 0
      var sumNumberOfVisitors: Int = 0
      bookings.foreach(b => {
        sumPrice += b.bookingPrice
        sumDiscount += b.discount
        sumProfitMargin += b.profitMargin
        sumNumberOfVisitors += b.noOfPeople
      })

      // Calculate avg value
      // Avoid getting length on each criterion
      val dataLength: Int = bookings.size
      val avgBookingPrice: Float = sumPrice / dataLength
      val avgDiscount: Float = sumDiscount / dataLength
      val avgProfitMargin: Float = sumProfitMargin / dataLength

      (avgBookingPrice, avgDiscount, avgProfitMargin, sumNumberOfVisitors)
    })
    // name, city, country, price, discount, profitMargin, numberOfVisitors
    .map(b => BookingForNormalization(
      b._1._1,
      b._1._2,
      b._1._3,
      b._2._1,
      b._2._2,
      b._2._3,
      b._2._4
    ))

    // Prepare min / max for normalization on each criterion
    // Use minBy / maxBy to avoid creating new collection if using map and min / max
    val priceRange = NormalizationRange[Float](
      bookingDataForNormalization.minBy(_.price).price,
      bookingDataForNormalization.maxBy(_.price).price
    )
    val discountRange = NormalizationRange[Float](
      bookingDataForNormalization.minBy(_.discount).discount,
      bookingDataForNormalization.maxBy(_.discount).discount
    )
    val profitMarginRange = NormalizationRange[Float](
      bookingDataForNormalization.minBy(_.profitMargin).profitMargin,
      bookingDataForNormalization.maxBy(_.profitMargin).profitMargin
    )
    val numberOfVisitorRange = NormalizationRange[Int](
      bookingDataForNormalization.minBy(_.numberOfVisitors).numberOfVisitors,
      bookingDataForNormalization.maxBy(_.numberOfVisitors).numberOfVisitors
    )

    // Normalization on each criterion
    bookingDataForNormalization.map(b => {
      val priceScore = normalizeValueByRange(b.price, priceRange.min, priceRange.max)
      val discountScore = normalizeValueByRange(b.discount, discountRange.min, discountRange.max)
      val profitMarginScore = normalizeValueByRange(
        b.profitMargin,
        profitMarginRange.min,
        profitMarginRange.max
      )
      val numberOfVisitorScore = normalizeValueByRange(
        b.numberOfVisitors,
        numberOfVisitorRange.min,
        numberOfVisitorRange.max
      )

      BookingScore(
        b.name,
        b.city,
        b.country,
        priceScore,
        discountScore,
        profitMarginScore,
        numberOfVisitorScore
      )
    })

  // Get most profitable hotel when considering the number of visitors and profit margin
  def getMostProfitableHotel(bookingScoreList: View[BookingScore]): ProfitableHotel =
    // Calculate avg score for economical hotel
    val avgScorePerHotel = bookingScoreList.map(b => {
      val avgScore = (b.numberOfVisitorScore + b.profitMarginScore) / 2

      ProfitableHotel(
        b.name,
        b.city,
        b.country,
        avgScore,
        b.numberOfVisitorScore,
        b.profitMarginScore
      )
    })

    // Get the largest avg score for profitable hotel
    avgScorePerHotel.maxBy(_.avgScore)

  // Finding the most economical hotel based on the 3 criteria
  // Booking Price, Discount, Profit Margin
  def getMostEconomistHotel(bookingScoreList: View[BookingScore]): EconomistHotel =
    // Calculate avg score for profitable hotel
    val avgScorePerHotel = bookingScoreList.map(b => {
      // Need to invert price and profit margin
      // because lowest value is the most economist for customers
      val invertPrice = 1 - b.priceScore
      val invertProfitMargin = 1 - b.profitMarginScore
      // Calculate avg score
      val avgScore = (invertPrice + b.discountScore + invertProfitMargin) / 3

      EconomistHotel(
        b.name,
        b.city,
        b.country,
        avgScore,
        invertPrice,
        b.discountScore,
        invertProfitMargin
      )
    })

    // Get the largest avg score for profitable hotel
    avgScorePerHotel.maxBy(_.avgScore)

  override def showAnalysis(content: IPrintAnalysis): Unit =
    content match {
      case HighestBookingCountry(name, numberOfBookings) =>
        println(
          s"""1. Country has the highest number of booking
            |Country: $name
            |Number of booking: $numberOfBookings
            |""".stripMargin)
      case EconomistHotel(name, city, country, avgScore, priceScore, discountScore, profitMarginScore) =>
        println(
          s"""2. Most economical hotel
             |Hotel: $name ($city, $country)
             |Booking price score (%): ${
              NumberUtil.formatFloatValue(NumberUtil.convertNumberToPercentage(priceScore))}
             |Discount score (%): ${
              NumberUtil.formatFloatValue(NumberUtil.convertNumberToPercentage(discountScore))}
             |Profit margin score (%): ${
              NumberUtil.formatFloatValue(NumberUtil.convertNumberToPercentage(profitMarginScore))}
             |Average score (%): ${
              NumberUtil.formatFloatValue(NumberUtil.convertNumberToPercentage(avgScore))}
             |""".stripMargin)
      case ProfitableHotel(name, city, country, avgScore, numberOfVisitorScore, profitMarginScore) =>
        println(
          s"""3. Most profitable Hotel
             |Hotel: $name ($city, $country)
             |Number of visitor score (%): ${
              NumberUtil.formatFloatValue(NumberUtil.convertNumberToPercentage(numberOfVisitorScore))}
             |Profit margin score (%): ${
              NumberUtil.formatFloatValue(NumberUtil.convertNumberToPercentage(profitMarginScore))}
             |Avg score (%): ${
              NumberUtil.formatFloatValue(NumberUtil.convertNumberToPercentage(avgScore))}
             |""".stripMargin)
    }
