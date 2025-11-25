import scala.collection.mutable.ListBuffer

// Parent print class for every case such as
// highest booking country, economist hotel, and profitable hotel
trait IPrintAnalysis

// Use parametric to reuse for multiple classes (future purposes), not only hotel booking analysis
trait IAnalysis[T]:
  def getList: List[T]
  // Able to count the highest number for any key
  def countHighestNumberPerKey(key: T => String): (String, Int)
  // Show analysis information
  def showAnalysis(content: IPrintAnalysis): Unit

// Used for showing analysis information on each case
case class HighestBookingCountry(name: String, value: Int) extends IPrintAnalysis
case class EconomistHotel(name: String, value: Float) extends IPrintAnalysis

class HotelBookingAnalysis extends CsvUtil, IAnalysis[Booking]:
  private val analysisDataList: ListBuffer[Booking] = new ListBuffer()

  override def parseCsvData(dataList: LazyList[List[String]]): Unit =
    dataList.foreach(data => {
      analysisDataList += new Booking(
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
        data(21).replace("%", "").toFloat,
        data(22).toFloat,
        data(23).toFloat
      )
    })

  // Convert buffer list to list for aggregation
  override def getList: List[Booking] =
    analysisDataList.toList

  // Show analysis information
//  override def showAnalysis(content: String): Unit =
//    println(content)

  // Get country has the highest number of booking
  // String: country name, Int: number of booking
  override def countHighestNumberPerKey(key: Booking => String): (String, Int) =
    val dataList: List[Booking] = getList

    // Group by destination country and count number of booking on each country
    val numberOfBookingPerCountryList = dataList.groupBy(key)
      // Count number of booking on each country
      .view.mapValues(_.size)
      // Sort descending
      .toList.sortBy(_._2).reverse

    // Get first record
    numberOfBookingPerCountryList.head

  def getHighestBookingCountry(key: Booking => String): HighestBookingCountry =
    val (country, numberOfBooking) = countHighestNumberPerKey(key)

    HighestBookingCountry(country, numberOfBooking)

  // Finding the most economical hotel based on the 3 criteria
  // Booking Price, Discount, Profit Margin
  def getMostEconomicalHotel: EconomistHotel =
    val dataList: List[Booking] = getList

    // Step 1: Use groupMapReduce for highly efficient aggregation
    val avgProfitScorePerHotel = dataList.groupMapReduce(_.hotel.hotelName)(
      // Map: (Profit Score, 1: used for counting record)
      b => (b.profitScore, 1)
    )(
      // Reduce: Sum (Profit Score) and Sum (Count)
      (total, next) => (total._1 + next._1, total._2 + next._2)
    ).view.mapValues:
      // Final calculation: Average profitScore = Total Score / Total Count
      case (totalScore, count) => totalScore / count.toFloat

    // Step 2: Sort ascending (lowest profitScore = most economical)
    // and get the economist hotel
    val sortedList = avgProfitScorePerHotel.toList.sortBy(_._2)
    // Return the most economist hotel (Hotel Name, Avg Profit Score)
    val (hotelName, avgProfitScore) = sortedList.head

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
            |Most economist hotel: $name
            |Average profit score: $value SGD
            |""".stripMargin)
    }
