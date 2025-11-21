import scala.collection.mutable.ListBuffer

// Use parametric to reuse for multiple classes (future purposes), not only hotel booking analysis
trait IAnalysis[T]:
  def getList: List[T]
  def showAnalysis(content: String): Unit

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

  // Get country has the highest number of booking
  // String: country name, Int: number of booking
  def getCountryHighestNumberOfBooking: (String, Int) =
    val dataList: List[Booking] = getList

    // Group by destination country and count number of booking on each country
    val numberOfBookingPerCountryList = dataList.groupBy(_.destinationCountry)
      // Count number of booking on each country
      .view.mapValues(_.size)
      // Sort descending
      .toList.sortBy(_._2).reverse

    // Get first record
    numberOfBookingPerCountryList.head

  // Show analysis information
  override def showAnalysis(content: String): Unit =
    println(content)
