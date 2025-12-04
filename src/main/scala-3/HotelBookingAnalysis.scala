// Parent print class for every case such as
// highest booking country, economist hotel, and profitable hotel
trait IPrintAnalysis

// Use parametric to reuse for multiple classes (future purposes), not only hotel booking analysis
trait IAnalysis[T]:
  // Show analysis information
  def showAnalysis(content: IPrintAnalysis): Unit

  // Get min value by key, remove private if other features use it
  private def getMinValueByKey(dataList: List[T], key: T => Float): Float =
    dataList.map(key).min

  // Get max value by key, remove private if other features use it
  private def getMaxValueByKey(dataList: List[T], key: T => Float): Float =
    dataList.map(key).max

  // Calculate avg value, remove private if other features use it
  private def calculateAvgValueByKey(dataList: List[T], key: T => Float): Float =
    val dataLength = dataList.size
    if dataLength == 0 then return 0.0

    dataList.map(key).sum / dataLength

  // Normalization based on min and max, remove private if other features use it
  private def normalizeValueByMinMax(value: Float, min: Float, max: Float): Float =
    // Avoid dividing by zero
    if min == max then return 0.0
    (value - min) / (max - min)

  // Get normalization score by key
  def getScoreByKey(dataList: List[T], key: T => Float): Float =
    // Prepare min, max by key for normalization
    val minValue: Float = getMinValueByKey(dataList, key)
    val maxValue: Float = getMaxValueByKey(dataList, key)

    // Calculate avg value by key
    val avgValue: Float = calculateAvgValueByKey(dataList, key)

    // Normalize value
    normalizeValueByMinMax(avgValue, minValue, maxValue)

// Used for showing analysis information on each case
case class HighestBookingCountry(name: String, numberOfBookings: Int) extends IPrintAnalysis
case class EconomistHotel(
                            name: String,
                            avgScore: Float,
                            priceScore: Float,
                            discountScore: Float,
                            profitMarginScore: Float) extends IPrintAnalysis
case class ProfitableHotel(
                            name: String,
                            avgScore: Float,
                            numberOfVisitors: Int,
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

  // Get most profitable hotel when considering the number of visitors and profit margin
  // String: hotel name, Int: total number of visitors, Float: total profits
  def getMostProfitableHotel: ProfitableHotel =
    val profitableHotel = analysisDataList.groupBy(_.hotel.hotelName).view.mapValues(bookings => {
      // Normalization profit margin
      val profitMarginScore: Float = getScoreByKey(bookings, _.profitMargin)

      // Total number of visitors
      val numberOfVisitors: Int = bookings.map(_.noOfPeople).sum

      // Calculate avg score
      val avgScore = (numberOfVisitors + profitMarginScore) / 2

      (avgScore, numberOfVisitors, profitMarginScore)
    })
    // Convert result into one object
    // (Hotel name, avg score, number of visitors, profit margin score)
    .map(b => (b._1, b._2._1, b._2._2, b._2._3))
    // Sort avg score descending -> most profitable for hotel
    .toList.sortBy(_._2).reverse

    val (name, avgScore, numberOfVisitors, profitMarginScore) = profitableHotel.head

    ProfitableHotel(name, avgScore, numberOfVisitors, profitMarginScore)

  // Get country has the highest number of booking
  // String: country name, Int: number of booking
  def getHighestBookingCountry: HighestBookingCountry =
    // Group by destination country and count number of booking on each country
    val numberOfBookingPerCountryList = analysisDataList
      // Group by a specific key such as destination_country and count on each value
      .groupMapReduce(_.destinationCountry)(_ => 1)(_ + _)
      // Sort count value descending
      .toList.sortBy(_._2).reverse

    val (country, numberOfBooking) = numberOfBookingPerCountryList.head

    HighestBookingCountry(country, numberOfBooking)

  // Finding the most economical hotel based on the 3 criteria
  // Booking Price, Discount, Profit Margin
  def getMostEconomistHotel: EconomistHotel =
    // Get min, max, avg value on the 3 criteria
    val avgScorePerHotel = analysisDataList.groupBy(_.hotel.hotelName).view.mapValues(bookings => {
      // Normalization on the 3 criteria
      val bookingPriceScore: Float = getScoreByKey(bookings, _.bookingPrice)
      val discountScore: Float = getScoreByKey(bookings, _.discount)
      val profitMarginScore: Float = getScoreByKey(bookings, _.profitMargin)

      // Invert price score and profit margin score -> higher is better for customers
      val invertBookingPriceScore: Float = 1 - bookingPriceScore
      val invertProfitMarginScore: Float = 1 - profitMarginScore

      // Avg score
      val avgScore: Float = (invertBookingPriceScore + discountScore + invertProfitMarginScore) / 3

      (avgScore, invertBookingPriceScore, discountScore, invertProfitMarginScore)
    })
    // Convert result into one object
    // (Hotel name, avg score, price score, discount score, profit margin score)
    .map(b => (b._1, b._2._1, b._2._2, b._2._3, b._2._4))
    // Sort avg score descending
    // Highest value is the most economical hotel for customers because we invert price and profit margin
    .toList.sortBy(_._2).reverse

    val (name, avgScore, priceScore, discountScore, profitMarginScore) = avgScorePerHotel.head

    EconomistHotel(name, avgScore, priceScore, discountScore, profitMarginScore)

  override def showAnalysis(content: IPrintAnalysis): Unit =
    content match {
      case HighestBookingCountry(name, numberOfBookings) =>
        println(
          s"""1. Country has the highest number of booking
            |Country: $name
            |Number of booking: $numberOfBookings
            |""".stripMargin)
      case EconomistHotel(name, avgScore, priceScore, discountScore, profitMarginScore) =>
        println(
          s"""2. Most economical hotel
             |Hotel: $name
             |Booking price score (%): ${
              NumberUtil.formatFloatValue(NumberUtil.convertNumberToPercentage(priceScore))}
             |Discount score (%): ${
              NumberUtil.formatFloatValue(NumberUtil.convertNumberToPercentage(discountScore))}
             |Profit margin score (%): ${
              NumberUtil.formatFloatValue(NumberUtil.convertNumberToPercentage(profitMarginScore))}
             |Average score (%): ${
              NumberUtil.formatFloatValue(NumberUtil.convertNumberToPercentage(avgScore))}
             |""".stripMargin)
      case ProfitableHotel(name, avgScore, numberOfVisitors, profitMarginScore) =>
        println(
          s"""3. Most profitable Hotel
             |Hotel: $name
             |Total visitors: $numberOfVisitors
             |Profit margin score: ${NumberUtil.formatFloatValue(profitMarginScore)}
             |Avg score: ${NumberUtil.formatFloatValue(avgScore)}
             |""".stripMargin)
    }
