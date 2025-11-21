// Group assignment - Hotel Booking Analysis
object Assignment2:
  def main(args: Array[String]): Unit =
    val analysis = new HotelBookingAnalysis

    // Read csv and load data
    val FILE_NAME = "Hotel_Dataset.csv"
    val dataList: LazyList[List[String]] = analysis.readCsv(
      getClass.getResource("/data/" + FILE_NAME).getPath
    )

    // Check empty data
    if dataList.isEmpty then
      println("No data for analysis")
    else
      // Parse and converting data to a specific class
      analysis.parseCsvData(dataList)

      // Close stream after transforming data
      analysis.closeStream()

      // Get booking data
      val bookingList = analysis.getList
      // TODO: remove this debug code
//      bookingList.foreach(booking => println(booking.bookingId))
      val counts = bookingList.groupBy(_.hotel.hotelName).view.mapValues(_.size)
      counts.foreach(println)
    end if
