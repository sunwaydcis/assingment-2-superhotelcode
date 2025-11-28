
// All utilities, such as formating value
trait IUtility:
  def formatFloatValue(value: Float, delimiter: Int = 3): Float =
    var d = 1
    for (i <- 1 to delimiter) { d *= 10 }
    // e.g. delimiter = 2 -> 123.23, 3 -> 123.235
    Math.round(value * d) / d.toFloat

// Parent print class for every case such as
// highest booking country, economist hotel, and profitable hotel
trait IPrintAnalysis

// Use parametric to reuse for multiple classes (future purposes), not only hotel booking analysis
trait IAnalysis[T]:
  // Able to count the highest number for any key
  def countHighestNumberPerKey(key: T => String): (String, Int)
  // Show analysis information
  def showAnalysis(content: IPrintAnalysis): Unit

// Used for showing analysis information on each case
case class HighestBookingCountry(name: String, value: Int) extends IPrintAnalysis
case class EconomistHotel(name: String, value: Float) extends IPrintAnalysis
case class ProfitableHotel(name: String, numberOfVisitors: Int, profit: Float) extends IPrintAnalysis

class HotelBookingAnalysis extends CsvUtil[Booking], IAnalysis[Booking], IUtility:
  private var analysisDataList: List[Booking] = List.empty

  override def readCsv(filename: String): List[Booking] =
    val dataList = super.readCsv(filename)
    analysisDataList = dataList

    // Assign local var to proceed with aggregation
    analysisDataList

  override def parseCsvData(data: List[String]): Booking =
    new Booking(
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
  // String: country name, Int: number of booking
  override def countHighestNumberPerKey(key: Booking => String): (String, Int) =
    // Group by destination country and count number of booking on each country
    val numberOfBookingPerCountryList = analysisDataList
      // Group by a specific key such as destination_country and count on each value
      .groupMapReduce(key)(_ => 1)(_ + _)
      // Sort count value descending
      .toList.sortBy(_._2).reverse

    // Get first record
    numberOfBookingPerCountryList.head

  // Get most profitable hotel when considering the number of visitors and profit margin
  // String: hotel name, Int: total number of visitors, Float: total profits
  def getMostProfitableHotel: ProfitableHotel =
    // Using groupMapReduce for single pass aggregation
    val profitableHotel = analysisDataList.groupMapReduce(
      // Grouping by hotel name
      _.hotel.hotelName
    )(
      // Map number of visitors and total profit on each hotel
      b => (b.noOfPeople, b.totalProfitScore)
    )(
      // Sum total number of visitors and total profit
      (total, next) =>(total._1 + next._1, total._2 + next._2)
    )
    // Convert result into one object. e.g. ("hotel 1", 12, 312.55)
    .map(b => (b._1, b._2._1, b._2._2))
    // Sort total profit descending
    // Highest: profitable hotel
    .toList.sortBy(_._2).reverse

    val (name, numberOfVisitors, profit) = profitableHotel.head

    ProfitableHotel(name, numberOfVisitors, profit)

  def getHighestBookingCountry: HighestBookingCountry =
    val (country, numberOfBooking) = countHighestNumberPerKey(_.destinationCountry)

    HighestBookingCountry(country, numberOfBooking)

  // Finding the most economical hotel based on the 3 criteria
  // Booking Price, Discount, Profit Margin
  def getMostEconomistHotel: EconomistHotel =
    // Step 1: Use groupMapReduce for highly efficient aggregation
    val avgProfitScorePerHotel = analysisDataList.groupMapReduce(_.hotel.hotelName)(
      // Map: (Profit Score, 1: used for counting record)
      b => (b.profitScore, 1)
    )(
      // Reduce: Sum (Profit Score) and Count booking per hotel
      (total, next) => (total._1 + next._1, total._2 + next._2)
    ).view.mapValues:
      // Final calculation: Average profitScore = Total Score / Total Count
      case (totalScore, count) => totalScore / count.toFloat
    // Step 2: Sort ascending (lowest profitScore = most economical)
    .toList.sortBy(_._2)

    // Return the most economist hotel (Hotel Name, Avg Profit Score)
    val (hotelName, avgProfitScore) = avgProfitScorePerHotel.head

    EconomistHotel(hotelName, avgProfitScore)

  override def showAnalysis(content: IPrintAnalysis): Unit =
    content match {
      case HighestBookingCountry(name, value) =>
        println(
          s"""1. Country has the highest number of booking
            |Country: $name
            |Number of booking: $value
            |""".stripMargin)
      case EconomistHotel(name, value) =>
        println(
          s"""2. Most economical hotel (lowest average profit score)
            |Hotel: $name
            |Average profit score (SGD): ${formatFloatValue(value)}
            |""".stripMargin)
      case ProfitableHotel(name, numberOfVisitors, profit) =>
        println(
          s"""3. Most profitable Hotel when considering the number of visitor and profit margin
             |Hotel: $name
             |Total Visitors: $numberOfVisitors
             |Total Profit (SGD): ${formatFloatValue(profit)}
             |""".stripMargin)
    }
